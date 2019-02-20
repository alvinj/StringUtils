pipeline {
    agent any

    stages {
        stage('Build') {
            steps {
                echo "Compiling..."
                sh "/usr/local/bin/sbt compile"
            }
        }
    }
}

