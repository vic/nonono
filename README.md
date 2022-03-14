# NoNoNo

An Scala3 compiler plugin that lets you prevent unsafe function calls.

### Setup

TODO: release jars.
TODO: usage with sbt
TODO: usage with mill

```scala
scalac -Xplugin:nonono.jar Something.scala
```

### Configuration

Suppose you want to prevent developers using `Option.get` on your codebase.

The following NoNoNo definition prevents such cases:

```scala
NoNoNo[Option[Any]](_.get)("Prefer using getOrElse")
```

<img width="1134" alt="Screen Shot 2022-03-12 at 10 43 58" src="https://user-images.githubusercontent.com/331/158126035-1d43cb14-6d1c-43a1-a4d0-951634e4bcca.png">


TODO: currently, the plugin expects NoNoNo definitions on the same compilation unit. Make it possible for the plugin to take an option with a scala file to read definitions from.

You can customize the type parameter to match for example, only `Option[String]`.

See more examples in [tests](nonono-plugin/tests/src/nonono/PluginTest.scala)
