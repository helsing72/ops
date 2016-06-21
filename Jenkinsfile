node ('humbug'){ //Bug in node restriction JENKINS-34377  
  def errors = []
   
  stage 'Checkout'
   // Get code from repository
  checkout([$class: 'GitSCM', branches: [[name: 'development']], doGenerateSubmoduleConfigurations: false, extensions: [], submoduleCfg: [], userRemoteConfigs: [[url: 'ssh://git@linus1.dynamics.saab.se:7999/cmw/ops4.git']]])
  try { //Ensure that reporting to Bitbucket is done even if the job fails
      
   stage 'Make'
    try {
        sh '''#!/bin/bash -l
            uname -a
            id
            #module add gcc/5.3.0
            module add cppcheck/1.68
            module add boost/1.60
            module add google-test/1.7
            module add gcovr/3.2
            module add cmake/3.1.0
 
            make -j4 | tee make_log
            '''
    } catch (e) {
        errors << "Make script failed: ${e}"
        echo "Error mesasge: ${errors}"
    } 

    stage "Archiving artifacts"
    if (errors.size() > 0) {
        currentBuild.result = 'FAILED'   
        echo "Errors exist, not archiving artifacts"
    } else {
        archive 'deploy/**'
    }

    //stage 'SDS build'
    //build job: 'sds_build_master', wait: false
    
 } finally {
    report_to_BitBucket()
    if (readFile('make_log').contains("warning")){
        echo "Contains warnings setting result to unstable"
        currentBuild.result = "UNSTABLE"
    }
    enableClaiming()
    sh 'rm -rf *' //Clean workspace after each execution
 }
}