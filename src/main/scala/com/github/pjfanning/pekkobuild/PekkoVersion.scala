package com.github.pjfanning.pekkobuild

import sbt.CrossVersion

sealed trait PekkoVersion {
  val majorVersion: Long
  def major: String = majorVersion.toString

  val minorVersion: Long
  def minor: String = minorVersion.toString

  val patchVersion: Long
  def patch: String = patchVersion.toString

  def binary: String = s"$major.$minor"

  def isSnapshot: Boolean

  def isDirty: Boolean

  def value: String = s"$binary.$patch"
}

object PekkoVersion {
  final case class Release(majorVersion: Long,
                           minorVersion: Long,
                           patchVersion: Long,
                           override val patch: String,
                           isSnapshot: Boolean,
                           isDirty: Boolean
  ) extends PekkoVersion

  final case class ReleaseCandidate(majorVersion: Long,
                                    minorVersion: Long,
                                    patchVersion: Long,
                                    override val patch: String,
                                    releaseCandidateVersion: Long,
                                    isSnapshot: Boolean,
                                    isDirty: Boolean
  ) extends PekkoVersion

  final case class Milestone(majorVersion: Long,
                             minorVersion: Long,
                             patchVersion: Long,
                             override val patch: String,
                             milestoneVersion: Long,
                             isSnapshot: Boolean,
                             isDirty: Boolean
  ) extends PekkoVersion

  private val PatchRegex            = """^(\d+).(\d+).(\d+)([a-zA-Z0-9_+-]*)$""".r
  private val ReleaseCandidateRegex = """^-RC(\d+)(.*)$""".r
  private val MilestoneRegex        = """^-M(\d+)(.*)$""".r

  def apply(version: String): PekkoVersion = {

    val (majorVersion, minorVersion, patchVersion, rest) = version match {
      case PatchRegex(majorVersion, minorVersion, patchVersion, rest) =>
        (majorVersion.toLong, minorVersion.toLong, patchVersion.toLong, rest)
    }

    val patch = s"$patchVersion$rest"

    val snapshot = patch.endsWith("-SNAPSHOT")

    rest match {
      case MilestoneRegex(mVersion, mRest) =>
        PekkoVersion.Milestone(majorVersion,
                               minorVersion,
                               patchVersion,
                               patch,
                               mVersion.toLong,
                               snapshot,
                               mRest.nonEmpty
        )
      case ReleaseCandidateRegex(rcVersion, rcRest) =>
        PekkoVersion.ReleaseCandidate(majorVersion,
                                      minorVersion,
                                      patchVersion,
                                      patch,
                                      rcVersion.toLong,
                                      snapshot,
                                      rcRest.nonEmpty
        )
      case _ =>
        PekkoVersion.Release(majorVersion, minorVersion, patchVersion, patch, snapshot, rest.nonEmpty)
    }
  }
}
