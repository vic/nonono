package nonono

import dotty.tools.dotc

import dotc.ast.Trees.*
import dotc.ast.{Trees, tpd, untpd}
import dotc.core.Constants.Constant
import dotc.core.Contexts.{Context, ContextBase}
import dotc.core.Decorators.*
import dotc.core.Phases.Phase
import dotc.core.{Mode, Names, Phases, Types}
import dotc.core.StdNames.*
import dotc.core.Symbols.*
import dotc.interfaces.SourceFile
import dotc.plugins.{PluginPhase, StandardPlugin}
import dotc.{CompilationUnit, report}
import dotc.sbt.ExtractDependencies
import dotc.transform.{
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
import dotty.tools.dotc.util.{ScriptSourceFile, SrcPos}
import dotty.tools.repl.{
  Command,
  Newline,
  ParseResult,
  Parsed,
  ReplCompilationUnit,
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

  class Compiler(pluginPhases: List[PluginPhase]) extends dotc.Compiler {
    override protected def transformPhases =
      super.transformPhases ++ List(pluginPhases)
    override protected def backendPhases = Nil
  }

class Plugin extends StandardPlugin:
  override val name: String = "nonono"
  override val description: String = "Prevent calling some methods"
  override def init(options: List[String]): List[PluginPhase] =
    val noDefs: Plugin.NoDefs = collection.mutable.Set.empty
    List(
      new Config(false, noDefs, options),
      new Define(noDefs),
      new Detect(noDefs)
    )

object Config:
  val name = "nonono-config"

class Config(
    var initialized: Boolean,
    noDefs: Plugin.NoDefs,
    options: List[String]
) extends PluginPhase:
  override val phaseName: String = Config.name
  override val runsAfter = Plugin.runsAfter
  override val runsBefore = Set(Define.name) ++ Plugin.runsBefore

  println(s"NEW CONFIG ${initialized}")

  override def run(using ctx: Context): Unit = {
    if (!initialized) {
      initialized = true
      foo(ctx)
    }
    super.run(using ctx)
  }

  def foo(parentCtx: Context): Unit = {
    val base = new ContextBase
    val ctx = base.initialCtx.fresh
    val x = parentCtx.settings.allSettings.map(s =>
      s.name -> s.valueIn(parentCtx.settingsState)
    )
    println(s"SETTINGS ${x}")
    ctx.setSettings(parentCtx.settingsState)
    ctx.base.initialize()(using ctx)
    ctx.base.usePhases(
      ctx.base.fusePhases(
        phasess = ctx.base.phasePlan,
        phasesToSkip = Nil,
        stopBeforePhases = Nil,
        stopAfterPhases = List(Define.name),
        YCheckAfter = Nil
      )(using ctx),
      fuse = false
    )

    val compiler =
      new Plugin.Compiler(
        List(
          new Config(true, noDefs, options),
          new Define(noDefs)
        )
      )
    compiler
      .newRun(using ctx)
      .compileFromStrings(
        scalaSources = """
                         |import nonono.NoNoNo
                         |val x = NoNoNo[Option[Any]](_.get)("NOP")
                         |""".stripMargin :: Nil,
        javaSources = Nil
      )
    println("FOOO")
  }

object Define:
  val name = "nonono-define"

class Define(noDefs: Plugin.NoDefs) extends PluginPhase:
  import tpd.*

  override val phaseName: String = Define.name
  override val runsAfter = Set(Config.name) ++ Plugin.runsAfter
  override val runsBefore = Set(Detect.name) ++ Plugin.runsBefore

  override def transformApply(tree: Apply)(using ctx: Context): Tree = {
    val nonoClass = requiredClass("nonono.NoNoNo")
    val applyName = Names.termName("apply")
    tree match {
      case Apply(
            Apply(TypeApply(Select(nonoIns, `applyName`), List(onType)), args),
            msgArgs
          ) if nonoIns.tpe <:< nonoClass.typeRef =>
        addNoNoDefinition(tree, onType, args, msgArgs)
        Trees.genericEmptyTree
      case other =>
        other
    }
  }

  def addNoNoDefinition(
      tree: Tree,
      onType: Tree,
      args: List[Tree],
      msgArgs: List[Tree]
  )(using
      ctx: Context
  ): Unit = {
    val anonName = Names.termName("$anonfun")
    (args, msgArgs) match {
      case (
            List(
              Block(
                List(
                  DefDef(`anonName`, List(List(ValDef(_, _, _))), _, app)
                ),
                _
              )
            ),
            List(Literal(msg))
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
        appTree.fold(reportInvalidUsage(tree.srcPos)) { app =>
          noDefs.addOne(Plugin.NoDef(onType, app, msg.stringValue))
        }
      case _ =>
        reportInvalidUsage(tree.srcPos)
    }
  }

  def reportInvalidUsage(pos: SrcPos)(using context: Context): Unit =
    report.error(
      """
        |Invalid NoNoNo usage.
        |First argument must be an anonymous function calling something you want to forbid.
        |Second argument must be a message, a literal string with no interpolations.
        |
        | Valid usage example: `NoNoNo[Option[Any]](_.get)("Please don't")`
        |""".stripMargin,
      pos = pos
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
