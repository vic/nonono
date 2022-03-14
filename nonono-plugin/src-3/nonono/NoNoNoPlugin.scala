package nonono

import dotty.tools.backend.jvm.GenBCode
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.{Names, Types}
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.interfaces.SourceFile
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.report
import dotty.tools.dotc.sbt.ExtractDependencies
import dotty.tools.dotc.transform.{
  CountOuterAccesses,
  Erasure,
  ExtensionMethods,
  Getters,
  LambdaLift,
  Pickler,
  PostTyper,
  Staging
}
import dotty.tools.dotc.typer.TyperPhase
import dotty.tools.dotc.util.ScriptSourceFile
import dotty.tools.repl.{
  Command,
  Newline,
  ParseResult,
  Parsed,
  SigKill,
  State,
  SyntaxErrors
}

import scala.annotation.unchecked.uncheckedVariance

object Plugin:
  type NoDefs = collection.mutable.Set[NoDef]

  val runsAfter: Set[String] = Set(ExtensionMethods.name)
  val runsBefore: Set[String] = Set(Erasure.name)

  case class NoDef(
      onType: Trees.Tree[Types.Type],
      app: Trees.Tree[Types.Type],
      msg: String
  )

class Plugin extends StandardPlugin:
  val name: String = "nonono"
  override val description: String = "Prevent calling some methods"
  def init(options: List[String]): List[PluginPhase] =
    val noDefs: Plugin.NoDefs = collection.mutable.Set.empty
    List(new Define(noDefs), new Detect(noDefs))

object Define:
  val name = "nonono-define"

class Define(noDefs: Plugin.NoDefs) extends PluginPhase:
  import tpd.*

  override val phaseName: String = Define.name
  override val runsAfter = Plugin.runsAfter
  override val runsBefore = Set(Detect.name) ++ Plugin.runsBefore

  override def transformApply(
      tree: Trees.Apply[Types.Type]
  )(using ctx: Context): Tree = {
    val nonoClass = requiredClass("nonono.NoNoNo")
    val applyName = Names.termName("apply")
    val anonName = Names.termName("$anonfun")

    tree match {
      case Apply(
            Apply(TypeApply(Select(nonoIns, `applyName`), List(onType)), args),
            List(Literal(msg)) // Todo dont capture literal here
          ) if nonoIns.tpe <:< nonoClass.typeRef =>
        args match {
          case List(
                Block(
                  List(DefDef(`anonName`, List(List(ValDef(_, _, _))), _, app)),
                  _
                )
              ) =>
            val appTree =
              app match {
                case x: Trees.Lazy[Trees.Apply[Types.Type] @unchecked] =>
                  Some(x.complete(using ctx))
                case x: Trees.Apply[Types.Type @unchecked] =>
                  Some(x)
                case x: Trees.Select[Types.Type @unchecked] =>
                  Some(x)
                case _ =>
                  None
              }
            appTree.fold(reportInvalidUsage(tree)) { app =>
              println(s"GOT ${app}")
              noDefs.addOne(Plugin.NoDef(onType, app, msg.stringValue))
            }
          case _ =>
            reportInvalidUsage(tree)
        }
        Trees.genericEmptyTree
      case other =>
        other
    }
  }

  def reportInvalidUsage(
      tree: Trees.Tree[Types.Type]
  )(using context: Context): Unit =
    println(tree)
    report.error(
      """
        |Invalid NoNoNo usage.
        |First argument must be an anonymous function calling something you want to forbid.
        |Second argument must be a message, a literal string with no interpolations.
        |
        | Valid usage example: `NoNoNo[Option[Any]](_.get)("Please don't")`
        |""".stripMargin,
      pos = tree.srcPos
    )

object Detect:
  val name = "nonono-detect"

class Detect(noDefs: Plugin.NoDefs) extends PluginPhase:
  import tpd.*

  override val phaseName: String = Detect.name
  override val runsAfter = Set(Define.name) ++ Plugin.runsAfter
  override val runsBefore = Plugin.runsBefore

  def sameSelect(
      noDef: Plugin.NoDef
  )(a: Select, b: Select)(using context: Context): Option[Plugin.NoDef] = {
    Option.when(
      a.name == b.name && a.qualifier.tpe <:< noDef.onType.tpe
    )(noDef)
  }

  def sameApply(
      noDef: Plugin.NoDef
  )(a: Apply, b: Apply)(using context: Context): Option[Plugin.NoDef] =
    (a, b) match {
      case (Apply(aFn, aArgs), Apply(bFn, bArgs))
          if a.applyKind == b.applyKind && aArgs.size == bArgs.size &&
            aArgs
              .zip(bArgs)
              .forall { case (a, b) => a.tpe <:< b.tpe } =>
        sameCall(noDef)(aFn, bFn)
      case _ => None
    }

  def sameCall(
      noDef: Plugin.NoDef
  )(a: Tree, b: Tree)(using context: Context): Option[Plugin.NoDef] =
    (a, b) match {
      case (a: Select, b: Select) => sameSelect(noDef)(a, b)
      case (a: Apply, b: Apply)   => sameApply(noDef)(a, b)
      case _                      => None
    }

  def prepareForCall(tree: Tree)(using ctx: Context): Context =
    noDefs.find(x =>
      sameCall(x)(tree, x.app).fold(false) { m =>
        report.restrictionError(m.msg, pos = tree.srcPos)
        true
      }
    )
    ctx

  override def prepareForSelect(tree: Select)(using ctx: Context): Context =
    prepareForCall(tree)

  override def prepareForApply(tree: Apply)(using ctx: Context): Context =
    prepareForCall(tree)
