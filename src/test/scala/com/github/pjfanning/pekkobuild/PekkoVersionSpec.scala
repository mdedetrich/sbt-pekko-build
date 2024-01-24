package com.github.pjfanning.pekkobuild

import org.scalatest.Inside.inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PekkoVersionSpec extends AnyWordSpec with Matchers {
  "PekkoVersion" should {
    "apply Milestone" in {
      val fullVersion = "1.2.3-M0+252-19da7367-SNAPSHOT"
      inside(PekkoVersion(fullVersion)) {
        case milestone: PekkoVersion.Milestone =>
          milestone.majorVersion shouldEqual 1
          milestone.minorVersion shouldEqual 2
          milestone.patchVersion shouldEqual 3
          milestone.isSnapshot shouldEqual true
          milestone.patch shouldEqual "3-M0+252-19da7367-SNAPSHOT"
          milestone.milestoneVersion shouldEqual 0
          milestone.binary shouldEqual "1.2"
          milestone.value shouldEqual fullVersion
        case _ =>
          fail(s"Expected $fullVersion to be a Milestone")
      }
    }
    "apply Release Candidate" in {
      val fullVersion = "1.2.3-RC0+252-19da7367-SNAPSHOT"
      inside(PekkoVersion(fullVersion)) {
        case releaseCandidate: PekkoVersion.ReleaseCandidate =>
          releaseCandidate.majorVersion shouldEqual 1
          releaseCandidate.minorVersion shouldEqual 2
          releaseCandidate.patchVersion shouldEqual 3
          releaseCandidate.isSnapshot shouldEqual true
          releaseCandidate.patch shouldEqual "3-M0+252-19da7367-SNAPSHOT"
          releaseCandidate.releaseCandidateVersion shouldEqual 0
          releaseCandidate.binary shouldEqual "1.2"
          releaseCandidate.value shouldEqual fullVersion
        case _ =>
          fail(s"Expected $fullVersion to be a Release Candidate")
      }
    }
  }

}
