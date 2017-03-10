library(randomForest)

# Definition of Helper functions for HMM training

# creates labels for the comments with the thresholder
labelCommentsWithTresholder <- function(project, comments) {
  outputBugCommentsHigh = str_replace_all(system(paste("java -jar rectanglelearner.jar ", project, "/bug_comments_thresholds_high.csv", sep=""), intern=TRUE), ",", ".")
  outputBugCommentsLow  = str_replace_all(system(paste("java -jar rectanglelearner.jar ", project, "/bug_comments_thresholds_low.csv", sep=""), intern=TRUE), ",", ".")
  
  for (k in 1:nrow(comments)) {
    if (comments$nocomm[k] <= as.numeric(strsplit(outputBugCommentsHigh[2], " ")[[1]][3])) {
      comments$state[k] = "low"
    }
    else if (comments$nocomm[k] <= as.numeric(strsplit(outputBugCommentsLow[2], " ")[[1]][3])) {
      comments$state[k] = "medium"
    }
    else {
      comments$state[k] = "high"
    }
  }
  return(comments)
}

# creates labels for the communication with the thresholder
labelCommunicationWithTresholder <- function(project, communication) {
  outputCommunicationHigh = str_replace_all(system(paste("java -jar rectanglelearner.jar ", project, "/communication_thresholds_sum_high.csv", sep=""), intern=TRUE), ",", ".")
  outputCommunicationLow  = str_replace_all(system(paste("java -jar rectanglelearner.jar ", project, "/communication_thresholds_sum_low.csv", sep=""), intern=TRUE), ",", ".")
  
  for (k in 1:nrow(communication)) {
    if (communication$activities[k] <= as.numeric(strsplit(outputCommunicationHigh[2], " ")[[1]][3])) {
      communication$state[k] = "low"
    }
    else if (communication$activities[k] <= as.numeric(strsplit(outputCommunicationLow[2], " ")[[1]][3])) {
      communication$state[k] = "medium"
    }
    else {
      communication$state[k] = "high"
    }
  }
  return(communication)
}

# creates labels for the contributions with the thresholder
labelContributionsWithTresholder <- function(project, contributions) {
  outputContributionsHigh = str_replace_all(system(paste("java -jar rectanglelearner.jar ", project, "/contribution_thresholds_high.csv", sep=""), intern=TRUE), ",", ".")
  outputContributionsLow  = str_replace_all(system(paste("java -jar rectanglelearner.jar ", project, "/contribution_thresholds_low.csv", sep=""), intern=TRUE), ",", ".")
  
  for (k in 1:nrow(contributions)) {
    if (contributions$dnoc[k]     <= as.numeric(strsplit(outputContributionsHigh[3], " ")[[1]][3]) &&
        contributions$bugfixes[k] <= as.numeric(strsplit(outputContributionsHigh[3], " ")[[1]][4])) {
      contributions$state[k] = "low"
    }
    else if (contributions$dnoc[k]     <= as.numeric(strsplit(outputContributionsLow[3], " ")[[1]][3]) &&
             contributions$bugfixes[k] <= as.numeric(strsplit(outputContributionsLow[3], " ")[[1]][4])) {
      contributions$state[k] = "medium"
    }
    else {
      contributions$state[k] = "high"
    }
  }
  return(contributions)
}

# labeling with KNN for comments
labelCommentsWithKNN <- function(project, comments, k) {
  bugCommentsHigh = read.csv(paste(project, "/bug_comments_thresholds_high.csv", sep=""))
  bugCommentsLow  = read.csv(paste(project, "/bug_comments_thresholds_low.csv", sep=""))
  bugComments = data.frame(m1=bugCommentsHigh[,1])
  for( i in 1:nrow(bugComments) ) {
    if( bugCommentsHigh[i,2]==0 ) {
      bugComments$class[i] = "low"
    }
    else if( bugCommentsLow[i,2]==1 ) {
      bugComments$class[i] = "high"
    }
    else {
      bugComments$class[i] = "medium"
    }
  }
  comments$state = knn(data.frame(bugComments$m1), data.frame(comments$nocomm), bugComments$class, k=k)
  return(comments)
}

# labeling with KNN for contributions
labelContributionsWithKNN <- function(project, contributions, k) {
  contributionsHigh = read.csv(paste(project, "/contribution_thresholds_high.csv", sep=""))
  contributionsLow  = read.csv(paste(project, "/contribution_thresholds_low.csv", sep=""))
  contributionsPreLabeled = data.frame(dnoc=contributionsHigh[,1], bugfixes=contributionsLow[,2])
  for( i in 1:nrow(contributionsPreLabeled) ) {
    if( contributionsHigh[i,3]==0 ) {
      contributionsPreLabeled$class[i] = "low"
    }
    else if( contributionsLow[i,3]==1 ) {
      contributionsPreLabeled$class[i] = "high"
    }
    else {
      contributionsPreLabeled$class[i] = "medium"
    }
  }
  contributions$state = knn(contributionsPreLabeled[,1:2], contributions[,3:4], contributionsPreLabeled$class, k=k)
  return(contributions)
}

# labeling with KNN for communication
labelCommunicationWithKNN <- function(project, communication, k) {
  communicationHigh = read.csv(paste(project, "/communication_thresholds_sum_high.csv", sep=""))
  communicationLow  = read.csv(paste(project, "/communication_thresholds_sum_low.csv", sep=""))
  communicationPreLabeled = data.frame(m1=communicationHigh[,1])
  for( i in 1:nrow(communicationPreLabeled) ) {
    if( communicationHigh[i,2]==0 ) {
      communicationPreLabeled$class[i] = "low"
    }
    else if( communicationLow[i,2]==1 ) {
      communicationPreLabeled$class[i] = "high"
    }
    else {
      communicationPreLabeled$class[i] = "medium"
    }
  }
  communication$state = knn(data.frame(communicationPreLabeled$m1), data.frame(communication$activities), communicationPreLabeled$class, k=k)
  return(communication)
}

# creates the overall state as the average of the communication, contribution and bug commenting state
calculateState = function (state1,state2,state3) {
  if(state1 == "low") {
    state1 = 1
  }
  if(state2 == "low") {
    state2 = 1
  }
  if(state3 == "low") {
    state3 = 1
  }
  if(state1 == "medium") {
    state1 = 2
  }
  if(state2 == "medium") {
    state2 = 2
  }
  if(state3 == "medium") {
    state3 = 2
  }
  if(state1 == "high") {
    state1 = 3
  }
  if(state2 == "high") {
    state2 = 3
  }
  if(state3 == "high") {
    state3 = 3
  }
  
  tmp = state1+state2+state3
  
  if(tmp<5){
    stateLabel = "low"
  }
  if(tmp<7 & tmp>4){
    stateLabel = "medium"
  }
  if(tmp>6){
    stateLabel = "high"
  }
  return(stateLabel)
}

# creates the initial emission matrix from observations (incl. covariance)
estimatePar = function (obs1,obs2,obs3,obs4, indexSet1, indexSet2, indexSet3) {
  obs1mu1 = mean(obs1[indexSet1])
  obs1mu2 = mean(obs1[indexSet2])
  obs1mu3 = mean(obs1[indexSet3])
  obs1sigma1 = var(obs1[indexSet1])
  obs1sigma2 = var(obs1[indexSet2])
  obs1sigma3 = var(obs1[indexSet3])
  
  obs2mu1 = mean(obs2[indexSet1])
  obs2mu2 = mean(obs2[indexSet2])
  obs2mu3 = mean(obs2[indexSet3])
  obs2sigma1 = var(obs2[indexSet1])
  obs2sigma2 = var(obs2[indexSet2])
  obs2sigma3 = var(obs2[indexSet3])
  
  obs3mu1 = mean(obs3[indexSet1])
  obs3mu2 = mean(obs3[indexSet2])
  obs3mu3 = mean(obs3[indexSet3])
  obs3sigma1 = var(obs3[indexSet1])
  obs3sigma2 = var(obs3[indexSet2])
  obs3sigma3 = var(obs3[indexSet3])
  
  obs4mu1 = mean(obs4[indexSet1])
  obs4mu2 = mean(obs4[indexSet2])
  obs4mu3 = mean(obs4[indexSet3])
  obs4sigma1 = var(obs4[indexSet1])
  obs4sigma2 = var(obs4[indexSet2])
  obs4sigma3 = var(obs4[indexSet3])
  
  cov1 = cov(matrix(c(obs1[indexSet1], obs2[indexSet1], obs3[indexSet1], obs4[indexSet1]), ncol=4))
  cov2 = cov(matrix(c(obs1[indexSet2], obs2[indexSet2], obs3[indexSet2], obs4[indexSet2]), ncol=4))
  cov3 = cov(matrix(c(obs1[indexSet3], obs2[indexSet3], obs3[indexSet3], obs4[indexSet3]), ncol=4))
  
  obslist = list(c(obs1mu1, obs2mu1, obs3mu1, obs4mu1), c(obs1mu2, obs2mu2, obs3mu2, obs4mu2), c(obs1mu3, obs2mu3, obs3mu3, obs4mu3))
  sigmalist = list(cov1,cov2,cov3)
  if (length(indexSet3)==0) {
    obslist = obslist[-3]
    sigmalist = sigmalist[-3]
  }
  if (length(indexSet2)==0) {
    obslist = obslist[-2]
    sigmalist = sigmalist[-2]
  }
  if (length(indexSet1)==0) {
    obslist = obslist[-1]
    sigmalist = sigmalist[-1]
  }
  
  emis = list(mu = obslist, sigma = sigmalist)
  
  return(emis)
}

# creates the initial state distribution
setInit = function(indexSet1, indexSet2, indexSet3) {
  init = c(0.8,0.15,0.05)
  if (length(indexSet3)==0) {
    init = init[-3]
  }
  if (length(indexSet2)==0) {
    init = init[-2]
  }
  if (length(indexSet1)==0) {
    init = init[-1]
  }
  if( sum(init)!=1 ) {
    init = init/sum(init)
  }
  return(init)
}

# creates the initial transaction matrix
setTrans = function(indexSet1, indexSet2, indexSet3) {
  trans = t(matrix(c(0.6,0.3,0.1,
                     0.3,0.5,0.2,
                     0.1,0.4,0.5),3))
  if (length(indexSet3)==0) {
    trans = as.data.frame(trans[-3,])
    trans = as.data.frame(trans[,-3])
  }
  if (length(indexSet2)==0) {
    trans = as.data.frame(trans[-2,])
    trans = as.data.frame(trans[,-2])
  }
  if (length(indexSet1)==0) {
    trans = as.data.frame(trans[-1,])
    trans = as.data.frame(trans[,-1])
  }
  for( i in 1:ncol(trans) ) {
    if( sum(trans[i,])!=1 ) {
      trans[i,] = trans[i,]/sum(trans[i,])
    }
  }
  return(trans)
}

# update function for the emmision matrix
mstep.mvnorm = function(x, wt) {
  emission = list(mu = list(), sigma = list())
  print(x)
  print(wt)
  for(i in 1:ncol(wt)) {
    tmp = cov.wt(x, wt[,i])
    emission$mu[[i]] = tmp$center
    emission$sigma[[i]] = tmp$cov
  }
  return(emission)
}

# draws j values from a multivariate normal distribution
rmvnorm.hsmm = function(j, model) rmvnorm(1, model$parms.emission$mu[[j]], model$parms.emission$sigma[[j]])

# distribtion function of a multivariate normal distribution
dmvnorm.hsmm = function(x, j, model) dmvnorm(x, model$parms.emission$mu[[j]], model$parms.emission$sigma[[j]])

# outputs correlation and KL-div between HMM models
corAndKL <- function(model1, model2, project1, project2, type) {
  corRes = cor.test(model1$transition, model2$transition)
  print(paste("  correlation between project", project1, "and", project2,"for", type, "models:", corRes$estimate))
}
