library(HMM)
library(lattice)
library(ggplot2)
library(mhsmm)
library(MASS)
library(fitdistrplus)
library(stringr)
library(class)
library(monomvn)

# !!! UPDATE PATH !!!
setwd("C:/Users/vh_material_promise2016")

source("hmm-helpers.R")

CLEANUP = TRUE
#CLEANUP = FALSE

projects = c("amarok","ant", "egit", "konsole", "log4j", "poi")

coreDevelopers <- c("dev1","dev2","dev3", "dev4","dev5","dev6", "dev7")
majorDevelopers <- c("dev8","dev9","dev10","dev11","dev12","dev13","dev14","dev15","dev16","dev17","dev18","dev19", "dev20", "dev21", "dev22", "dev23", "dev24", "dev25", "dev26", "dev27", "dev28", "dev29", "dev30", "dev31", "dev32", "dev33","dev34","dev35","dev36","dev37","dev38","dev39","dev40","dev41","dev42","dev43","dev44","dev45","dev46", "dev110", "dev47")
minorDevelopers <- c("dev48","dev49","dev50","dev51","dev52","dev53","dev54","dev55", "dev56", "dev57", "dev58", "dev59", "dev60", "dev61", "dev62", "dev63", "dev64", "dev65","dev66","dev67","dev68","dev69","dev70","dev71","dev72","dev73" )

initClassifiers = c("thresholder", "knn")

knn_k = 1

error = data.frame(type=character(), initClassifier=character(), project=character(), developerName=character(), curError=numeric(), stringsAsFactors = FALSE)
devstats = data.frame(initClassifier=character(), project=character(), developerName=character(), nBugs=numeric(), nComm=numeric(), nContr=numeric(), working=logical(), stringsAsFactors = FALSE)
initComparison = data.frame(project=character(), diff_comments=numeric(), diff_communication=numeric(), diff_contributions=numeric(), stringsAsFactors = FALSE)

coreModels = list()
majorModels = list()
minorModels = list()

for( projectIndex in 1:length(projects)) {
  project = projects[projectIndex]
  print(paste("now starting for project", project))
  for( classifierIndex in 1:length(initClassifiers) ) {
    coreModel = NULL
    majorModel = NULL
    minorModel = NULL
    nCoreDevs = 0
    nMajorDevs = 0
    nMinorDevs = 0
    
    initClassifier = initClassifiers[classifierIndex]
    print(paste("  now starting for initial classifier", initClassifier))
    
    # 1) Load data
    contributions = read.csv(paste(project, "/contributions.csv", sep=""))
    communication = read.csv(paste(project, "/communication_summarized.csv", sep=""))
    comments = read.csv(paste(project, "/bug_comments.csv", sep=""))
    
    # 2) use pre-labeled data to train a classifier and label the rest of the data
    if( initClassifier=="thresholder" ) {
      comments = labelCommentsWithTresholder(project, comments)
      communication = labelCommunicationWithTresholder(project, communication)
      contributions = labelContributionsWithTresholder(project, contributions)
    } else if( initClassifier=="knn" ) {
      comments = labelCommentsWithKNN(project, comments, knn_k)
      communication = labelCommunicationWithKNN(project, communication, knn_k)
      contributions = labelContributionsWithKNN(project, contributions, knn_k)
    } else if( initClassifier=="knn3" ) {
      comments = labelCommentsWithKNN(project, comments, knn_k3)
      communication = labelCommunicationWithKNN(project, communication, knn_k3)
      contributions = labelContributionsWithKNN(project, contributions, knn_k3)
    } else if( initClassifier=="knn5" ) {
      comments = labelCommentsWithKNN(project, comments, knn_k5)
      communication = labelCommunicationWithKNN(project, communication, knn_k5)
      contributions = labelContributionsWithKNN(project, contributions, knn_k5)
    }
    else {
      stop(paste("initial classifier is unkown:", initClassifier))
    }
    
    # 3) train HMM model using labeled data over time for each developer
    
    # 3.1) determine timeframe where all data is available
    minYear = as.numeric(strsplit(min(as.character(contributions$month)),"-")[[1]][1])
    minMonth = as.numeric(strsplit(min(as.character(contributions$month)),"-")[[1]][2])
    maxYear = as.numeric(strsplit(max(as.character(contributions$month)),"-")[[1]][1])
    maxMonth = as.numeric(strsplit(max(as.character(contributions$month)),"-")[[1]][2])
    
    timespan = list()
    for (year in minYear:maxYear) {
      for (month in 1:12) {
        if ((year == minYear && month >= minMonth) ||
            (year > minYear && year < maxYear) ||
            (year == maxYear && month <= maxMonth)) {
          curYear = paste(year, str_pad(month, 2, pad = "0"), sep = "-")
          timespan = c(timespan, curYear)
        }
      }
    }
    
    developers = as.character(unique(contributions$name))
    
    for(devIndex in 1:length(developers)) {
      developerName = developers[devIndex]
      print(paste("    creating HMM for developer", developerName))
      
      # 3.2) consider only data from the current developer
      contributionStates =
        contributions[contributions$name == developerName,]
      bugStates = comments[comments$commentAuthor == developerName,]
      commStates = communication[communication$name == developerName,]
      
      contributionStates$month = as.character(contributionStates$month)
      bugStates$month = as.character(bugStates$month)
      commStates$month = as.character(commStates$month)
      
      contributionStates$name = as.character(contributionStates$name)
      bugStates$commentAuthor = as.character(bugStates$commentAuthor)
      commStates$name = as.character(commStates$name)
      
      devstats = rbind(devstats, list(initClassifier=initClassifier, project=project, developerName=developerName, nBugs=nrow(bugStates), nComm=nrow(commStates), nContr=nrow(contributionStates), working=TRUE))
      devstats$initClassifier = as.character(devstats$initClassifier)
      devstats$developerName = as.character(devstats$developerName)
      devstats$project = as.character(devstats$project)
      
      # 3.3) add low contribution behavior with 0 values for missing months
      for (year in minYear:maxYear) {
        for (month in 1:12) {
          if ((year == minYear && month >= minMonth) ||
              (year > minYear && year < maxYear) ||
              (year == maxYear && month <= maxMonth)) {
            curYear = paste(year, str_pad(month, 2, pad = "0"), sep = "-")
            if (!any(contributionStates$month == curYear)) {
              contributionStates =
                rbind(contributionStates,list(name=developerName,month=curYear,dnoc=0,bugfixes=0,state="low"))
              if( class(contributionStates$month)=="factor" ) {
                contributionStates$month = as.character(contributionStates$month)
              }
            }
            if (!any(bugStates$month == curYear)) {
              bugStates = rbind(bugStates,list(commentAuthor=developerName,month=curYear,nocomm=0,state="low"))
              if( class(bugStates$month)=="factor" ) {
                bugStates$month = as.character(bugStates$month)
              }
            }
            if (!any(commStates$month == curYear)) {
              commStates = rbind(commStates,list(name=developerName,month=curYear,opened=0,responses=0,activities=0,state="low"))
              if( class(commStates$month)=="factor" ) {
                commStates$month = as.character(commStates$month)
              }
            }
          }
        }
      }
      if( CLEANUP ) {
        rm(curYear, year, month)
      }
      
      # 3.4) order and combine data
      contributionStates =
        contributionStates[contributionStates$month %in% timespan,]
      contributionStates =
        contributionStates[order(contributionStates$month),]
      colnames(contributionStates)[5] = "state1"
      
      bugStates = bugStates[bugStates$month %in% timespan,]
      bugStates = bugStates[order(bugStates$month),]
      colnames(bugStates)[4] = "state2"
      
      commStates = commStates[commStates$month %in% timespan,]
      commStates = commStates[order(commStates$month),]
      colnames(commStates)[6] = "state3"
      
      classObs = cbind(contributionStates,bugStates,commStates)
      
      # 3.5) determine overall classification
      for (i in 1:nrow(classObs)) {
        classObs$stateLabel[i] =
          calculateState(classObs$state1[i],classObs$state2[i],classObs$state3[i])
      }
      if( CLEANUP ) {
        rm(i)
      }
      
      # 3.6) finally train HMM
      indexSet1 = sample(which(classObs$stateLabel == "low"))
      indexSet2 = sample(which(classObs$stateLabel == "medium"))
      indexSet3 = sample(which(classObs$stateLabel == "high"))
      
      obs1 = as.numeric(contributionStates$dnoc)
      obs2 = as.numeric(contributionStates$bugfixes)
      obs3 = as.numeric(commStates$activities)
      obs4 = as.numeric(bugStates$nocomm)
      
      init = setInit(indexSet1,indexSet2,indexSet3)
      trans = setTrans(indexSet1,indexSet2,indexSet3)
      emis = estimatePar(obs1,obs2,obs3,obs4,indexSet1,indexSet2,indexSet3)
      
      model =
        hmmspec(
          init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm
        )
      train =
        simulate(model, nsim = 100, seed = 123, rand.emis = rmvnorm.hsmm)
      
      # in case of linux, NUL must be changed to /dev/null of the capture.output()
      exitCode = try(capture.output(hmm <- hmmfit(train, model, mstep = mstep.mvnorm), file='NUL'), silent = TRUE)
      if( class(exitCode)=="try-error" ) {
        print(paste("     could not create HMM for developer", developerName))
        devstats$working[nrow(devstats)] = FALSE
      } else {
        # 3.7) use Viterbi algorithm to predict labels
        #print(hmm$model)
        N = as.numeric(length(contributionStates$dnoc))
        obs =
          cbind(
            as.numeric(contributionStates$dnoc),as.numeric(contributionStates$bugfixes),as.numeric(commStates$activities),as.numeric(bugStates$nocomm)
          )
        trainDev = list(x = obs, N = N)
        class(trainDev) = "hsmm.data"
        
        # 3.8) build average model
        if( developerName %in% coreDevelopers ) {
          nCoreDevs = nCoreDevs+1
          if( is.null(coreModel) ) {
            coreModel = hmm$model
          } else {
            coreModel$transition = ((nCoreDevs-1)*coreModel$transition+hmm$model$transition)/nCoreDevs
            coreModel$init = ((nCoreDevs-1)*coreModel$init+hmm$model$init)/nCoreDevs
            
            nEmis = length(coreModel$parms.emission$mu[[1]])
            A1 = diag(nEmis)*(nCoreDevs-1)
            A2 = diag(nEmis)/nCoreDevs
            for( i in 1:coreModel$J ) {
              if( i>hmm$model$J ){
                coreModel$parms.emission$mu[[i]] = A2%*%(A1%*%coreModel$parms.emission$mu[[i]])
                coreModel$parms.emission$sigma[[i]] = A2%*%(A1%*%coreModel$parms.emission$sigma[[i]]%*%A1)%*%A2
              } else {
                coreModel$parms.emission$mu[[i]] = A2%*%(A1%*%coreModel$parms.emission$mu[[i]]+hmm$model$parms.emission$mu[[i]])
                coreModel$parms.emission$sigma[[i]] = A2%*%(A1%*%coreModel$parms.emission$sigma[[i]]+hmm$model$parms.emission$sigma[[i]]%*%A1)%*%A2
              }
            }
            if( CLEANUP ) {
              rm(i, nEmis, A1, A2) 
            }
          }
        }
        if( developerName %in% majorDevelopers ) {
          nMajorDevs = nMajorDevs+1
          if( is.null(majorModel) ) {
            majorModel = hmm$model
            if( majorModel$J==1 ) {
              majorModel$J=3
              
              majorModel$transition = cbind(majorModel$transition, c(0))
              majorModel$transition = cbind(majorModel$transition, c(0))
              majorModel$transition = rbind(majorModel$transition, c(0,1,0))
              majorModel$transition = rbind(majorModel$transition, c(0,0,1))
              
              majorModel$init = c(majorModel$init, 0,0)
              
              majorModel$parms.emission$mu[[2]] = mat.or.vec(4,1)
              majorModel$parms.emission$sigma[[2]] = mat.or.vec(4,4)
              majorModel$parms.emission$mu[[3]] = mat.or.vec(4,1)
              majorModel$parms.emission$sigma[[3]] = mat.or.vec(4,4)
            }
            else if( majorModel$J==2) {
              majorModel$J = 3
              
              majorModel$transition = cbind(majorModel$transition, c(0,0))
              majorModel$transition = rbind(majorModel$transition, c(0,0,1))
              majorModel$init = c(majorModel$init, 0)
              
              majorModel$parms.emission$mu[[3]] = mat.or.vec(4,1)
              majorModel$parms.emission$sigma[[3]] = mat.or.vec(4,4)
            }
          } else {
            if( hmm$model$J==1 ) {
              # single state model
              print("  single state model - ignored for average model")
              tmpTrans = hmm$model$transition
              tmpTrans = cbind(tmpTrans, c(0))
              tmpTrans = cbind(tmpTrans, c(0))
              tmpTrans = rbind(tmpTrans, c(0,1,0))
              tmpTrans = rbind(tmpTrans, c(0,0,1))
              tmpInit = c(hmm$model$init, 0,0)
              majorModel$transition = ((nMajorDevs-1)*majorModel$transition+tmpTrans)/nMajorDevs
              majorModel$init = ((nMajorDevs-1)*majorModel$init+tmpInit)/nMajorDevs
              if( CLEANUP ) {
                rm(tmpTrans, tmpInit)
              }
            }
            else if( hmm$model$J==2) {
              tmpTrans = hmm$model$transition
              tmpTrans = cbind(tmpTrans, c(0,0))
              tmpTrans = rbind(tmpTrans, c(0,0,1))
              tmpInit = c(hmm$model$init, 0)
              majorModel$transition = ((nMajorDevs-1)*majorModel$transition+tmpTrans)/nMajorDevs
              majorModel$init = ((nMajorDevs-1)*majorModel$init+tmpInit)/nMajorDevs
              
              
              if( CLEANUP ) {
                rm(tmpTrans, tmpInit)
              }
            } else {
              majorModel$transition = ((nMajorDevs-1)*majorModel$transition+hmm$model$transition)/nMajorDevs
              majorModel$init = ((nMajorDevs-1)*majorModel$init+hmm$model$init)/nMajorDevs
            }
            nEmis = length(majorModel$parms.emission$mu[[1]])
            A1 = diag(nEmis)*(nMajorDevs-1)
            A2 = diag(nEmis)/nMajorDevs
            for( i in 1:majorModel$J ) {
              if( i>hmm$model$J ){
                majorModel$parms.emission$mu[[i]] = A2%*%(A1%*%majorModel$parms.emission$mu[[i]])
                majorModel$parms.emission$sigma[[i]] = A2%*%(A1%*%majorModel$parms.emission$sigma[[i]]%*%A1)%*%A2
              } else {
                majorModel$parms.emission$mu[[i]] = A2%*%(A1%*%majorModel$parms.emission$mu[[i]]+hmm$model$parms.emission$mu[[i]])
                majorModel$parms.emission$sigma[[i]] = A2%*%(A1%*%majorModel$parms.emission$sigma[[i]]+hmm$model$parms.emission$sigma[[i]]%*%A1)%*%A2
              }
            }
            if( CLEANUP ) {
              rm(i, nEmis, A1, A2) 
            }
          }
        }
        if( developerName %in% minorDevelopers ) {
          nMinorDevs = nMinorDevs+1
          if( is.null(minorModel) ) {
            minorModel = hmm$model
            if( minorModel$J==1 ) {
              minorModel$J = 3
              
              minorModel$transition = cbind(minorModel$transition, c(0))
              minorModel$transition = cbind(minorModel$transition, c(0))
              minorModel$transition = rbind(minorModel$transition, c(0,1,0))
              minorModel$transition = rbind(minorModel$transition, c(0,0,1))
              
              minorModel$init = c(minorModel$init, 0,0)
              
              minorModel$parms.emission$mu[[2]] = mat.or.vec(4,1)
              minorModel$parms.emission$sigma[[2]] = mat.or.vec(4,4)
              minorModel$parms.emission$mu[[3]] = mat.or.vec(4,1)
              minorModel$parms.emission$sigma[[3]] = mat.or.vec(4,4)
            }
            else if( minorModel$J==2) {
              minorModel$J = 3
              
              minorModel$transition = cbind(minorModel$transition, c(0,0))
              minorModel$transition = rbind(minorModel$transition, c(0,0,1))
              minorModel$init = c(minorModel$init, 0)
              
              minorModel$parms.emission$mu[[3]] = mat.or.vec(4,1)
              minorModel$parms.emission$sigma[[3]] = mat.or.vec(4,4)
            }
          } else {
            if( any(is.na(minorModel$init)||is.infinite(minorModel$init)) ) {
              stop("fooooo")
            }
            if( hmm$model$J==1 ) {
              tmpTrans = hmm$model$transition
              tmpTrans = cbind(tmpTrans, c(0))
              tmpTrans = cbind(tmpTrans, c(0))
              tmpTrans = rbind(tmpTrans, c(0,1,0))
              tmpTrans = rbind(tmpTrans, c(0,0,1))
              tmpInit = c(hmm$model$init, 0,0)
              minorModel$transition = ((nMinorDevs-1)*minorModel$transition+tmpTrans)/nMinorDevs
              minorModel$init = ((nMinorDevs-1)*minorModel$init+tmpInit)/nMinorDevs
              if( CLEANUP ) {
                rm(tmpTrans, tmpInit)
              }
            }
            else if( hmm$model$J==2) {
              tmpTrans = hmm$model$transition
              tmpTrans = cbind(tmpTrans, c(0,0))
              tmpTrans = rbind(tmpTrans, c(0,0,1))
              tmpInit = c(hmm$model$init, 0)
              minorModel$transition = ((nMinorDevs-1)*minorModel$transition+tmpTrans)/nMinorDevs
              minorModel$init = ((nMinorDevs-1)*minorModel$init+tmpInit)/nMinorDevs
              if( CLEANUP ) {
                rm(tmpTrans, tmpInit)
              }
            } else {
              minorModel$transition = ((nMinorDevs-1)*minorModel$transition+hmm$model$transition)/nMinorDevs
              minorModel$init = ((nMinorDevs-1)*minorModel$init+hmm$model$init)/nMinorDevs
            }
            nEmis = length(minorModel$parms.emission$mu[[1]])
            A1 = diag(nEmis)*(nMinorDevs-1)
            A2 = diag(nEmis)/nMinorDevs
            for( i in 1:minorModel$J ) {
              if( i>hmm$model$J ){
                minorModel$parms.emission$mu[[i]] = A2%*%(A1%*%minorModel$parms.emission$mu[[i]])
                minorModel$parms.emission$sigma[[i]] = A2%*%(A1%*%minorModel$parms.emission$sigma[[i]]%*%A1)%*%A2
              } else {
                minorModel$parms.emission$mu[[i]] = A2%*%(A1%*%minorModel$parms.emission$mu[[i]]+hmm$model$parms.emission$mu[[i]])
                minorModel$parms.emission$sigma[[i]] = A2%*%(A1%*%minorModel$parms.emission$sigma[[i]]+hmm$model$parms.emission$sigma[[i]]%*%A1)%*%A2
              }
            }
            if( CLEANUP ) {
              rm(i, nEmis, A1, A2) 
            }
          }
        }
        
        # 3.9) estimate error for developer
        hmmPred = predict(hmm, trainDev, method = "viterbi")
        
        for (i in 1:nrow(classObs)) {
          if (classObs$stateLabel[i] == "low")
          {
            classObs$labelNumeric[i] = 1
          }
          if (classObs$stateLabel[i] == "medium")
          {
            classObs$labelNumeric[i] = 2
          }
          if (classObs$stateLabel[i] == "high")
          {
            classObs$labelNumeric[i] = 3
          }
        }
        if( CLEANUP ) {
          rm(i)
        }
        
        curError = mean(hmmPred$s != classObs$labelNumeric)
        error = rbind(error, list(type="dev", initClassifier=initClassifier, project=project, developerName=developerName, error=curError))
        if( class(error$developerName)=="factor" ) {
          error$developerName = as.character(error$developerName)
        }
        if( class(error$project)=="factor" ) {
          error$project = as.character(error$project)
        }
        if( class(error$initClassifier)=="factor" ) {
          error$initClassifier = as.character(error$initClassifier)
        }
        if( class(error$type)=="factor" ) {
          error$type = as.character(error$type)
        }
        if( CLEANUP ) {
          rm(N, obs, trainDev, hmmPred, curError, hmm)
        }
      }
    }
    if( nCoreDevs>0 ) {
      coreModels[[length(coreModels)+1]] <- list(project=project, initClassifier=initClassifier, nDevs=nCoreDevs, model=coreModel)
    }
    if( nMajorDevs>0 ) {
      majorModels[[length(majorModels)+1]] <- list(project=project, initClassifier=initClassifier, nDevs=nMajorDevs, model=majorModel)
    }
    if( nMinorDevs>0 ) {
      minorModels[[length(minorModels)+1]] <- list(project=project, initClassifier=initClassifier, nDevs=nMinorDevs, model=minorModel)
    }
    
    if( CLEANUP ) {
      rm(devIndex, developers, developerName)
      rm(bugStates, contributionStates, commStates)
      rm(classObs, obs1, obs2, obs3, obs4)
      rm(indexSet1, indexSet2, indexSet3)
      rm(init, trans, emis, model, train, exitCode)
      rm(minYear, maxYear, minMonth, maxMonth, timespan)
      rm(initClassifier, comments, communication, contributions)
      rm(nCoreDevs, coreModel, nMajorDevs, majorModel, nMinorDevs, minorModel)
    }
  }
  
  contributions = read.csv(paste(project, "/contributions.csv", sep=""))
  communication = read.csv(paste(project, "/communication_summarized.csv", sep=""))
  comments = read.csv(paste(project, "/bug_comments.csv", sep=""))
  
  thr_comments = labelCommentsWithTresholder(project, comments)
  thr_communication = labelCommunicationWithTresholder(project, communication)
  thr_contributions = labelContributionsWithTresholder(project, contributions)
  
  knn_comments = labelCommentsWithKNN(project, comments, knn_k)
  knn_communication = labelCommunicationWithKNN(project, communication, knn_k)
  knn_contributions = labelContributionsWithKNN(project, contributions, knn_k)
  
  diff_comments = mean(thr_comments$state != knn_comments$state)
  diff_communication = mean(thr_communication$state != knn_communication$state)
  diff_contributions = mean(thr_contributions$state != knn_contributions$state)
  initComparison = rbind(initComparison, list(project=project, diff_comments=diff_comments, diff_communication=diff_communication, diff_contributions=diff_contributions))
  initComparison$project = as.character(initComparison$project)
  
  if( CLEANUP ) {
    rm(project, classifierIndex)
  }
}
if( CLEANUP ) {
  rm(contributions, communication, comments)
  rm(thr_comments, thr_communication, thr_contributions)
  rm(knn_comments, knn_communication, knn_contributions)
  rm(diff_comments, diff_communication, diff_contributions)
  rm(projectIndex)
}

# 4) Creating universal models
universalCoreModels = list()
universalMajorModels = list()
universalMinorModels = list()

for( classifierIndex in 1:length(initClassifiers) ) {
  initClassifier = initClassifiers[classifierIndex]
  print(paste("universal model for classifier", initClassifier))
  
  universalCoreModel = NULL
  universalMajorModel = NULL
  universalMinorModel = NULL
  nCoreDevs = 0
  nMajorDevs = 0
  nMinorDevs = 0
  
  # core developers
  for( coreIndex in 1:length(coreModels)) {
    if( coreModels[[coreIndex]]$initClassifier==initClassifier ) {
      nCoreDevs = nCoreDevs+1
      if( is.null(universalCoreModel) ) {
        universalCoreModel = coreModels[[coreIndex]]$model
      } else {
        universalCoreModel$transition = ((nCoreDevs-1)*universalCoreModel$transition+coreModels[[coreIndex]]$model$transition)/nCoreDevs
        universalCoreModel$init = ((nCoreDevs-1)*universalCoreModel$init+coreModels[[coreIndex]]$model$init)/nCoreDevs
        
        nEmis = length(universalCoreModel$parms.emission$mu[[1]])
        A1 = diag(nEmis)*(nCoreDevs-1)
        A2 = diag(nEmis)/nCoreDevs
        for( i in 1:universalCoreModel$J ) {
          universalCoreModel$parms.emission$mu[[i]] = A2%*%(A1%*%universalCoreModel$parms.emission$mu[[i]]+coreModels[[coreIndex]]$model$parms.emission$mu[[i]])
          universalCoreModel$parms.emission$sigma[[i]] = A2%*%(A1%*%universalCoreModel$parms.emission$sigma[[i]]+coreModels[[coreIndex]]$model$parms.emission$sigma[[i]]%*%A1)%*%A2
        }
        if( CLEANUP ) {
          rm(i, nEmis, A1, A2) 
        }
      }
    }
  }
  # major developers
  for( majorIndex in 1:length(majorModels)) {
    if( majorModels[[majorIndex]]$initClassifier==initClassifier ) {
      nMajorDevs = nMajorDevs+1
      if( is.null(universalMajorModel) ) {
        universalMajorModel = majorModels[[majorIndex]]$model
      } else {
        universalMajorModel$transition = ((nMajorDevs-1)*universalMajorModel$transition+majorModels[[majorIndex]]$model$transition)/nMajorDevs
        universalMajorModel$init = ((nMajorDevs-1)*universalMajorModel$init+majorModels[[majorIndex]]$model$init)/nMajorDevs
        
        nEmis = length(universalMajorModel$parms.emission$mu[[1]])
        A1 = diag(nEmis)*(nMajorDevs-1)
        A2 = diag(nEmis)/nMajorDevs
        for( i in 1:universalMajorModel$J ) {
          universalMajorModel$parms.emission$mu[[i]] = A2%*%(A1%*%universalMajorModel$parms.emission$mu[[i]]+majorModels[[majorIndex]]$model$parms.emission$mu[[i]])
          universalMajorModel$parms.emission$sigma[[i]] = A2%*%(A1%*%universalMajorModel$parms.emission$sigma[[i]]+majorModels[[majorIndex]]$model$parms.emission$sigma[[i]]%*%A1)%*%A2
        }
        if( CLEANUP ) {
          rm(i, nEmis, A1, A2) 
        }
      }
    }
  }
  # minor developers
  for( minorIndex in 1:length(minorModels)) {
    if( minorModels[[minorIndex]]$initClassifier==initClassifier ) {
      nMinorDevs = nMinorDevs+1
      if( is.null(universalMinorModel) ) {
        universalMinorModel = minorModels[[minorIndex]]$model
      } else {
        universalMinorModel$transition = ((nMinorDevs-1)*universalMinorModel$transition+minorModels[[minorIndex]]$model$transition)/nMinorDevs
        universalMinorModel$init = ((nMinorDevs-1)*universalMinorModel$init+minorModels[[minorIndex]]$model$init)/nMinorDevs
        
        nEmis = length(universalMinorModel$parms.emission$mu[[1]])
        A1 = diag(nEmis)*(nMinorDevs-1)
        A2 = diag(nEmis)/nMinorDevs
        for( i in 1:universalMinorModel$J ) {
          universalMinorModel$parms.emission$mu[[i]] = A2%*%(A1%*%universalMinorModel$parms.emission$mu[[i]]+minorModels[[minorIndex]]$model$parms.emission$mu[[i]])
          universalMinorModel$parms.emission$sigma[[i]] = A2%*%(A1%*%universalMinorModel$parms.emission$sigma[[i]]+minorModels[[minorIndex]]$model$parms.emission$sigma[[i]]%*%A1)%*%A2
        }
        if( CLEANUP ) {
          rm(i, nEmis, A1, A2) 
        }
      }
    }
  }
  
  universalCoreModels[[length(universalCoreModels)+1]] <- list(initClassifier=initClassifier, nProjs=nCoreDevs, model=universalCoreModel)
  universalMajorModels[[length(universalMajorModels)+1]] <- list(initClassifier=initClassifier, nProjs=nMajorDevs, model=universalMajorModel)
  universalMinorModels[[length(universalMinorModels)+1]] <- list(initClassifier=initClassifier, nProjs=nMinorDevs, model=universalMinorModel)
  
}
if( CLEANUP ) {
  rm(universalCoreModel, universalMajorModel, universalMinorModel)
  rm(nCoreDevs, nMajorDevs, nMinorDevs)
  rm(projectIndex, classifierIndex, coreIndex)
}

# 5) Calculating missclassification rate for the universal models
for( projectIndex in 1:length(projects)) {
  project = projects[projectIndex]
  print(paste("calculated missclassficiation rates for universal model for project", project))
  for( classifierIndex in 1:length(initClassifiers) ) {
    coreModel = NULL
    majorModel = NULL
    minorModel = NULL
    nCoreDevs = 0
    nMajorDevs = 0
    nMinorDevs = 0
    
    initClassifier = initClassifiers[classifierIndex]
    print(paste("  now starting for initial classifier", initClassifier))
    
    # 5.1) Load data
    contributions = read.csv(paste(project, "/contributions.csv", sep=""))
    communication = read.csv(paste(project, "/communication_summarized.csv", sep=""))
    comments = read.csv(paste(project, "/bug_comments.csv", sep=""))
    
    # 5.2) use pre-labeled data to train a classifier and label the rest of the data
    if( initClassifier=="thresholder" ) {
      comments = labelCommentsWithTresholder(project, comments)
      communication = labelCommunicationWithTresholder(project, communication)
      contributions = labelContributionsWithTresholder(project, contributions)
    } else if( initClassifier=="knn" ) {
      comments = labelCommentsWithKNN(project, comments, knn_k)
      communication = labelCommunicationWithKNN(project, communication, knn_k)
      contributions = labelContributionsWithKNN(project, contributions, knn_k)
    } else if( initClassifier=="knn3" ) {
      comments = labelCommentsWithKNN(project, comments, knn_k3)
      communication = labelCommunicationWithKNN(project, communication, knn_k3)
      contributions = labelContributionsWithKNN(project, contributions, knn_k3)
    } else if( initClassifier=="knn5" ) {
      comments = labelCommentsWithKNN(project, comments, knn_k5)
      communication = labelCommunicationWithKNN(project, communication, knn_k5)
      contributions = labelContributionsWithKNN(project, contributions, knn_k5)
    } else {
      stop(paste("initial classifier is unkown:", initClassifier))
    }
    
    # 5.3) train HMM model using labeled data over time for each developer
    
    # 5.4) determine timeframe where all data is available
    minYear = as.numeric(strsplit(min(as.character(contributions$month)),"-")[[1]][1])
    minMonth = as.numeric(strsplit(min(as.character(contributions$month)),"-")[[1]][2])
    maxYear = as.numeric(strsplit(max(as.character(contributions$month)),"-")[[1]][1])
    maxMonth = as.numeric(strsplit(max(as.character(contributions$month)),"-")[[1]][2])
    
    timespan = list()
    for (year in minYear:maxYear) {
      for (month in 1:12) {
        if ((year == minYear && month >= minMonth) ||
            (year > minYear && year < maxYear) ||
            (year == maxYear && month <= maxMonth)) {
          curYear = paste(year, str_pad(month, 2, pad = "0"), sep = "-")
          timespan = c(timespan, curYear)
        }
      }
    }
    
    developers = as.character(unique(contributions$name))
    
    for(devIndex in 1:length(developers)) {
      developerName = developers[devIndex]
      print(paste("    calcualting error for for developer", developerName))
      
      # 5.5) consider only data from the current developer
      contributionStates =
        contributions[contributions$name == developerName,]
      bugStates = comments[comments$commentAuthor == developerName,]
      commStates = communication[communication$name == developerName,]
      
      contributionStates$month = as.character(contributionStates$month)
      bugStates$month = as.character(bugStates$month)
      commStates$month = as.character(commStates$month)
      
      contributionStates$name = as.character(contributionStates$name)
      bugStates$commentAuthor = as.character(bugStates$commentAuthor)
      commStates$name = as.character(commStates$name)
      
      devstats = rbind(devstats, list(initClassifier=initClassifier, project=project, developerName=developerName, nBugs=nrow(bugStates), nComm=nrow(commStates), nContr=nrow(contributionStates), working=TRUE))
      devstats$initClassifier = as.character(devstats$initClassifier)
      devstats$developerName = as.character(devstats$developerName)
      devstats$project = as.character(devstats$project)
      
      # 5.6) add low contribution behavior with 0 values for missing months
      for (year in minYear:maxYear) {
        for (month in 1:12) {
          if ((year == minYear && month >= minMonth) ||
              (year > minYear && year < maxYear) ||
              (year == maxYear && month <= maxMonth)) {
            curYear = paste(year, str_pad(month, 2, pad = "0"), sep = "-")
            if (!any(contributionStates$month == curYear)) {
              contributionStates =
                rbind(contributionStates,list(name=developerName,month=curYear,dnoc=0,bugfixes=0,state="low"))
              if( class(contributionStates$month)=="factor" ) {
                contributionStates$month = as.character(contributionStates$month)
              }
            }
            if (!any(bugStates$month == curYear)) {
              bugStates = rbind(bugStates,list(commentAuthor=developerName,month=curYear,nocomm=0,state="low"))
              if( class(bugStates$month)=="factor" ) {
                bugStates$month = as.character(bugStates$month)
              }
            }
            if (!any(commStates$month == curYear)) {
              commStates = rbind(commStates,list(name=developerName,month=curYear,opened=0,responses=0,activities=0,state="low"))
              if( class(commStates$month)=="factor" ) {
                commStates$month = as.character(commStates$month)
              }
            }
          }
        }
      }
      if( CLEANUP ) {
        rm(curYear, year, month)
      }
      
      # 5.7) order and combine data
      contributionStates =
        contributionStates[contributionStates$month %in% timespan,]
      contributionStates =
        contributionStates[order(contributionStates$month),]
      colnames(contributionStates)[5] = "state1"
      
      bugStates = bugStates[bugStates$month %in% timespan,]
      bugStates = bugStates[order(bugStates$month),]
      colnames(bugStates)[4] = "state2"
      
      commStates = commStates[commStates$month %in% timespan,]
      commStates = commStates[order(commStates$month),]
      colnames(commStates)[6] = "state3"
      
      classObs = cbind(contributionStates,bugStates,commStates)
      
      # 5.8) determine overall classification
      for (i in 1:nrow(classObs)) {
        classObs$stateLabel[i] =
          calculateState(classObs$state1[i],classObs$state2[i],classObs$state3[i])
      }
      if( CLEANUP ) {
        rm(i)
      }
      
      hmm = NULL
      if( developerName %in% coreDevelopers ) {
        type = "core"
        for( coreIndex in 1:length(universalCoreModels)) {
          if( universalCoreModels[[coreIndex]]$initClassifier==initClassifier ) {
            hmm = universalCoreModels[[coreIndex]]$model
          }
        }
      } 
      else if( developerName %in% majorDevelopers ) {
        type = "major"
        for( majorIndex in 1:length(universalMajorModels)) {
          if( universalMajorModels[[majorIndex]]$initClassifier==initClassifier ) {
            hmm = universalMajorModels[[majorIndex]]$model
          }
        }
      }
      else if( developerName %in% minorDevelopers ) {
        type = "minor"
        for( minorIndex in 1:length(universalMinorModels)) {
          if( universalMinorModels[[minorIndex]]$initClassifier==initClassifier ) {
            hmm = universalMinorModels[[minorIndex]]$model
          }
        }
      }
      if( is.null(hmm) ) {
        type = "default"
        for( minorIndex in 1:length(universalMinorModels)) {
          if( universalMinorModels[[minorIndex]]$initClassifier==initClassifier ) {
            hmm = universalMinorModels[[minorIndex]]$model
          }
        }
      }
      if( CLEANUP ) {
        rm(coreIndex, majorIndex, minorIndex)
      }
      if( hmm$init[3]==0 ) {
        hmm$J = 2
        hmm$init = hmm$init[1:2]
        hmm$transition = hmm$transition[1:2,1:2]
        hmm$parms.emission$mu = hmm$parms.emission$mu[1:2]
        hmm$parms.emission$sigma = hmm$parms.emission$sigma[1:2]
      }
      
      # 5.9) use Viterbi algorithm to predict labels
      #print(hmm$model)
      N = as.numeric(length(contributionStates$dnoc))
      obs =
        cbind(
          as.numeric(contributionStates$dnoc),as.numeric(contributionStates$bugfixes),as.numeric(commStates$activities),as.numeric(bugStates$nocomm)
        )
      trainDev = list(x = obs, N = N)
      class(trainDev) = "hsmm.data"
      
      hmmPred = predict(hmm, trainDev, method = "viterbi")
      
      # 5.10) estimate error for developer
      for (i in 1:nrow(classObs)) {
        if (classObs$stateLabel[i] == "low")
        {
          classObs$labelNumeric[i] = 1
        }
        if (classObs$stateLabel[i] == "medium")
        {
          classObs$labelNumeric[i] = 2
        }
        if (classObs$stateLabel[i] == "high")
        {
          classObs$labelNumeric[i] = 3
        }
      }
      if( CLEANUP ) {
        rm(i)
      }
      
      curError = mean(hmmPred$s != classObs$labelNumeric)
      error = rbind(error, list(type=type, initClassifier=initClassifier, project=project, developerName=developerName, error=curError))
      if( class(error$developerName)=="factor" ) {
        error$developerName = as.character(error$developerName)
      }
      if( class(error$project)=="factor" ) {
        error$project = as.character(error$project)
      }
      if( class(error$initClassifier)=="factor" ) {
        error$initClassifier = as.character(error$initClassifier)
      }
      if( class(error$type)=="factor" ) {
        error$type = as.character(error$type)
      }
      if( CLEANUP ) {
        rm(N, obs, trainDev, hmmPred, curError, hmm)
      }
    }
  }
  if( CLEANUP ) {
    rm(devIndex, developers, developerName)
    rm(bugStates, contributionStates, commStates)
    rm(minYear, maxYear, minMonth, maxMonth, timespan)
    rm(initClassifier, comments, communication, contributions)
    rm(nCoreDevs, coreModel, nMajorDevs, majorModel, nMinorDevs, minorModel)
  }
}
if( CLEANUP ) {
  rm(contributions, communication, comments)
  rm(thr_comments, thr_communication, thr_contributions)
  rm(knn_comments, knn_communication, knn_contributions)
  rm(diff_comments, diff_communication, diff_contributions)
  rm(projectIndex, project, knn_k, knn_k3, knn_k5, type)
  rm(coreDevelopers, maintainerDevelopers, majorDevelopers, minorDevelopers)
}

# 5) Performing correlation analysis
for( classifierIndex in 1:length(initClassifiers) ) {
  initClassifier = initClassifiers[classifierIndex]
  print(paste("correlations for classifier", initClassifier))
  for( projectOuterIndex in 1:length(projects)) {
    project1 = projects[projectOuterIndex]
    if( projectOuterIndex<length(projects)) {
      for( projectInnerIndex in (projectOuterIndex+1):length(projects)) {
        project2 = projects[projectInnerIndex]
        # core developers
        model1 = NULL
        model2 = NULL
        for( coreIndex in 1:length(coreModels)) {
          if( coreModels[[coreIndex]]$project==project1 && coreModels[[coreIndex]]$initClassifier==initClassifier) {
            model1 = coreModels[[coreIndex]]$model
          }
          if( coreModels[[coreIndex]]$project==project2 && coreModels[[coreIndex]]$initClassifier==initClassifier) {
            model2 = coreModels[[coreIndex]]$model
          }
        }
        if( !is.null(model1) && !is.null(model2) ) {
          corAndKL(model1, model2, project1, project2,"core")
        }
        
        # major developers
        model1 = NULL
        model2 = NULL
        for( majorIndex in 1:length(majorModels)) {
          if( majorModels[[majorIndex]]$project==project1 && majorModels[[majorIndex]]$initClassifier==initClassifier ) {
            model1 = majorModels[[majorIndex]]$model
          }
          if( majorModels[[majorIndex]]$project==project2 && majorModels[[majorIndex]]$initClassifier==initClassifier ) {
            model2 = majorModels[[majorIndex]]$model
          }
        }
        if( !is.null(model1) && !is.null(model2) ) {
          corAndKL(model1, model2, project1, project2,"major")
        }
        
        # minor developers
        model1 = NULL
        model2 = NULL
        for( minorIndex in 1:length(minorModels)) {
          if( minorModels[[minorIndex]]$project==project1 && minorModels[[minorIndex]]$initClassifier==initClassifier ) {
            model1 = minorModels[[minorIndex]]$model
          }
          if( minorModels[[minorIndex]]$project==project2 && minorModels[[minorIndex]]$initClassifier==initClassifier ) {
            model2 = minorModels[[minorIndex]]$model
          }
        }
        if( !is.null(model1) && !is.null(model2) ) {
          corAndKL(model1, model2, project1, project2,"minor")
        }
      }
    }
  }
}
if( CLEANUP ) {
  rm(coreIndex, majorIndex, minorIndex)
  rm(model1, model2, corRes)
  rm(project1, project2, projectOuterIndex, projectInnerIndex)
  rm(initClassifier, classifierIndex)
  rm(projects, initClassifiers)
}

summary = error[error$type!="dev",]
colnames(summary)[length(colnames(summary))] <- "errorUniv"
summary$errorDev = NA
for( i in 1:nrow(summary) ) {
  errorTmp = error[error$type=="dev" & error$initClassifier==summary$initClassifier[i] & error$project==summary$project[i] & error$developerName==summary$developerName[i], ]
  if( nrow(errorTmp)==1) {
    summary$errorDev[i] = errorTmp$error
  }
  else if( nrow(errorTmp)>1) {
    print(i)
    print(errorTmp)
    stop("this should not happen, error data frame seems to be broken")
  }
}
if( CLEANUP ) {
  rm(i)
}

summary_thresholder = summary[summary$initClassifier=="thresholder",]
summary_knn = summary[summary$initClassifier=="knn",]
colnames(summary_thresholder)[length(colnames(summary_thresholder))-1] = "errorUnivThr"
colnames(summary_thresholder)[length(colnames(summary_thresholder))] = "errorDevThr"
summary = summary_thresholder[,c(1,3,4)]
summary$errorUnivThr = summary_thresholder$errorUniv
summary$errorDevThr = summary_thresholder$errorDev
summary$errorUnivKnn = summary_knn$errorUniv
summary$errorDevKnn = summary_knn$errorDev

if( CLEANUP ) {
  rm(summary_thresholder, summary_knn)
}

print(colMeans(summary[,4:ncol(summary)], na.rm=TRUE))

hmmCore = universalCoreModels[[1]]$model
hmmMajor = universalMajorModels[[1]]$model
hmmMinor = universalMinorModels[[1]]$model

nSim = 24

simCore = simulate(hmmCore, nsim = nSim, rand.emis = rmvnorm.hsmm)
simMajor1 = simulate(hmmMajor, nsim = nSim, rand.emis = rmvnorm.hsmm)
simMajor2 = simulate(hmmMajor, nsim = nSim, rand.emis = rmvnorm.hsmm)
simMinor1 = simulate(hmmMinor, nsim = nSim, rand.emis = rmvnorm.hsmm)
simMinor2 = simulate(hmmMinor, nsim = nSim, rand.emis = rmvnorm.hsmm)
simMinor3 = simulate(hmmMinor, nsim = nSim, rand.emis = rmvnorm.hsmm)
simMinor4 = simulate(hmmMinor, nsim = nSim, rand.emis = rmvnorm.hsmm)
simMinor5 = simulate(hmmMinor, nsim = nSim, rand.emis = rmvnorm.hsmm)

outputTotal = simCore$x+simMajor1$x+simMajor2$x+simMinor1$x+simMinor1$x+simMinor2$x+simMinor3$x+simMinor4$x+simMinor5$x

# plot example application

#commits 

core <- simCore$x[,1]
major <- (simMajor1$x[,1]+simMajor2$x[,1])/2
minor <- (simMinor1$x[,1]+simMinor2$x[,1]+simMinor3$x[,1]+simMinor4$x[,1]+simMinor5$x[,1])/5

month <- c(1:24)

library(reshape)

commits <- data.frame(month,core,major,minor)
commits <- melt(commits,id=c("month"))

print(ggplot(commits, aes(x=commits$month, y=commits$value, colour=commits$variable, group=commits$variable)) 
      #   + geom_point() 
      + geom_line(aes(commits$month, commits$value),size=2) 
      + scale_colour_manual(values=c("blue","red","orange"))
      + scale_y_continuous(name="number of commits")
      + scale_x_continuous(name="month")
      + guides(color=guide_legend(title="developer type")))


#states

core <- simCore[[1]]
major1 <- simMajor1[[1]]
major2 <- simMajor2[[1]]
minor1 <- simMinor1[[1]]
minor2 <- simMinor2[[1]]
minor3 <- simMinor3[[1]]
minor4 <- simMinor4[[1]]
minor5 <- simMinor5[[1]]

states <- data.frame(month,core,major1,major2,minor1,minor2,minor3,minor4,minor5)

for (i in 1:nrow(states)){
  states$major[i] <- (major1[i]+major2[i])/2
  states$minor[i] <- (minor1[i]+minor2[i]+minor2[i]+minor4[i]+minor5[i])/5
}

states <- states[,c(1,2,10,11)]

states <- melt(states,id=c("month"))


#dodged bar chart

print(ggplot(states, aes(x=states$month, y=states$value, fill=states$variable))
      + geom_bar(stat="identity", position="dodge")
      + scale_fill_manual(values=c("blue","red","orange"))
      + scale_y_continuous(name="state",breaks = c(1,2,3), labels = c("low", "medium", "high"))
      + scale_x_continuous(name="month")
      + guides(fill=guide_legend(title="developer type")))

# cretate boxplots

# multiple boxplots

boxplotdata <- summary[,c(2,5,7)]

colnames(boxplotdata)[2] <- "thresholder"
colnames(boxplotdata)[3] <- "knn"

boxplotdata <- melt(boxplotdata,id=c("project"))

p <- ggplot(data=boxplotdata, aes(x=factor(boxplotdata$project),y=boxplotdata$value,fill=boxplotdata$variable))
print(p + geom_boxplot() + xlab("project") + ylab("misclassification rate")
      + scale_y_continuous(limits = c(0,0.4))
      + scale_fill_manual(values=c("lightblue","#66CC99"))
      + guides(fill=guide_legend(title="classifier")))

# for universal model

boxplotuniversal <- summary[,c(2,4,6)]

colnames(boxplotuniversal)[2] <- "universal thresholder"
colnames(boxplotuniversal)[3] <- "universal knn"

boxplotuniversal <- melt(boxplotuniversal,id=c("project"))

p <- ggplot(data=boxplotuniversal, aes(x=factor(boxplotuniversal$project),y=boxplotuniversal$value,fill=boxplotuniversal$variable))
print(p + geom_boxplot() + xlab("project") + ylab("misclassification rate")
      + scale_y_continuous(limits = c(0,0.4))
      + scale_fill_manual(values=c("lightblue","#66CC99"))
      + guides(fill=guide_legend(title="classifier")))





