enablePlugins(ScalaJSPlugin)

// HRF_MAIN=hrf.VastSimEntry — симуляция Vast ботами (vast.Host.main), иначе браузерный клиент hrf.HRF
val hrfMainClass = sys.env.get("HRF_MAIN").map(_.trim).filter(_.nonEmpty).getOrElse("hrf.HRF")

Compile / mainClass := Some(hrfMainClass)

// Имя host.scala отсекает все такие файлы (в т.ч. старый vast/host); симуляция Vast — в vast/VastHost.scala
Compile / unmanagedSources / excludeFilter := "reflect-jvm.scala" || "log-jvm.scala" || "host-jvm.scala" || "grey-jvm.scala" || "timeline-jvm.scala" || "host.scala" || "convert-images.scala" || "extract-logs.scala"

scalaJSUseMainModuleInitializer := true

scalaJSLinkerConfig ~= { _.withOptimizer(false) }
// scalaJSLinkerConfig ~= { _.withOptimizer(true) }

// scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }

// Compile / fullLinkJS / scalaJSLinkerConfig ~= { _.withClosureCompiler(false) }

// Compile / unmanagedSourceDirectories += baseDirectory.value / "dom" / "scala"

// Compile / unmanagedSourceDirectories += baseDirectory.value / "dom" / "scala-2"

// Compile / unmanagedSourceDirectories += baseDirectory.value / "dom" / "scala-new-collections"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0-SNAPSHOT"
