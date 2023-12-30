/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * license agreements; and to You under the Apache License, version 2.0:
 *
 *   https://www.apache.org/licenses/LICENSE-2.0
 *
 * This file is part of the Apache Pekko project, derived from Akka.
 */

package com.github.pjfanning.pekkobuild

import sbt.plugins.JvmPlugin
import sbt._
import sbt.Keys._

object PekkoInlinePlugin extends AutoPlugin {
  override def trigger: PluginTrigger = allRequirements

  override def requires: Plugins = JvmPlugin

  object autoImport extends PekkoInlineSettings

  import autoImport._

  private def flagsForScala2(coreProject: Boolean) = {
    val baseInlineFlags = Seq(
      "-opt-inline-from:<sources>",
      "-opt:l:inline"
    )

    if (coreProject)
      baseInlineFlags ++ Seq(
        // Since the Pekko core project doesn't allow for mixing of different versions,
        // i.e. you cannot mix pekko-actor 1.0.0 with pekko-streams 1.0.1 at runtime
        // its safe to do inter sbt project inlining.
        "-opt-inline-from:org.apache.pekko.**"
      )
    else baseInlineFlags ++ Seq(
      // These are safe to inline even across modules since they are
      // wrappers for cross compilation that is stable within Pekko core.
      "-opt-inline-from:org.apache.pekko.dispatch.internal.SameThreadExecutionContext**",
      "-opt-inline-from:org.apache.pekko.util.OptionConverters**",
      "-opt-inline-from:org.apache.pekko.util.FutureConverters**",
      "-opt-inline-from:org.apache.pekko.util.FunctionConverters**",
      "-opt-inline-from:org.apache.pekko.util.PartialFunction**",
      "-opt-inline-from:org.apache.pekko.util.JavaDurationConverters**"
    )
  }

  // Optimizer not yet available for Scala3, see https://docs.scala-lang.org/overviews/compiler-options/optimizer.html
  private val flagsForScala3 = Seq()

  override lazy val globalSettings = Seq(
    pekkoInlineEnabled := !sys.props.contains("pekko.no.inline"),
    pekkoInlineCoreProject := false
  )

  override lazy val projectSettings = Seq(Compile / scalacOptions ++= {
    if (pekkoInlineEnabled.value) {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n == 12 | n == 13 =>
          flagsForScala2(pekkoInlineCoreProject.value)
        case Some((3, _)) =>
          flagsForScala3
      }
    } else Seq.empty
  })
}
