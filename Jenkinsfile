/* -*- mode: groovy -*-
  Confgure how to run our job in Jenkins.
  See https://github.com/castle-engine/castle-engine/wiki/Cloud-Builds-(Jenkins) .
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
        sh 'castle-engine package --os=win64 --cpu=x86_64 --verbose'
        sh 'castle-engine package --os=win32 --cpu=i386 --verbose'
        sh 'castle-engine package --os=linux --cpu=x86_64 --verbose'
      }
    }
  }
  post {
    success {
      archiveArtifacts artifacts: 'glplotter*.tar.gz,glplotter*.zip,glplotter*.apk'
    }
    regression {
      mail to: 'michalis.kambi@gmail.com',
        subject: "[jenkins] Build started failing: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    failure {
      mail to: 'michalis.kambi@gmail.com',
        subject: "[jenkins] Build failed: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    fixed {
      mail to: 'michalis.kambi@gmail.com',
        subject: "[jenkins] Build is again successfull: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
  }
}
