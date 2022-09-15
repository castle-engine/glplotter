/* -*- mode: groovy -*-
  Configure how to run our job in Jenkins.
  See https://castle-engine.io/cloud_builds_jenkins .
*/

pipeline {
  triggers {
    pollSCM('H/4 * * * *')
    upstream(upstreamProjects: 'castle_game_engine_update_docker_image', threshold: hudson.model.Result.SUCCESS)
  }
  agent {
    docker {
      image 'kambi/castle-engine-cloud-builds-tools:cge-unstable'
    }
  }
  stages {
    stage('Build Desktop') {
      steps {
        sh 'rm -f glplotter-*.tar.gz glplotter-*.zip glplotter*.apk'
        sh './pack.sh'
      }
    }
  }
  post {
    success {
      archiveArtifacts artifacts: 'glplotter*.tar.gz,glplotter*.zip,glplotter*.apk'
    }
    regression {
      mail to: 'michalis@castle-engine.io',
        subject: "[jenkins] Build started failing: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    failure {
      mail to: 'michalis@castle-engine.io',
        subject: "[jenkins] Build failed: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    fixed {
      mail to: 'michalis@castle-engine.io',
        subject: "[jenkins] Build is again successfull: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
  }
}
