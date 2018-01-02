library(plyr)
library(dplyr)
library(mvtnorm)

gen_model <- function(data, variables, n_groups, n_iter = 30, smooth_const = 0.02){
  priors = rep(1/n_groups, n_groups)
  initial_groups = sample(1:n_groups, nrow(data), replace=TRUE)
  data$group = initial_groups
  n_rows = nrow(data)
  gmean = list()
  gvar = list()
  for (g in 1:n_groups){
    subdata = data %>% filter(group==g)
    gmean[[g]] = as.numeric(colMeans(subdata[,variables]))
    gvar[[g]] = as.numeric(colwise(var)(subdata[,variables]))
  }
  #actual priors will not be set until during first iteration
  probs = matrix(nrow=nrow(data), ncol=n_groups)
  for (i in 1:n_iter){
    #calculate new probs
    for (g in 1:n_groups){
      probs[,g] = priors[g] * dmvnorm(data[,variables], mean=gmean[[g]], sigma = diag(gvar[[g]]))
      priors[g] = sum(data$group==g)/n_rows
    }
    #smoothing
    priors = (priors + smooth_const)/sum(priors + smooth_const)
    
    #reassign groups
    data$group = apply(probs, 1, which.max)
    
    #calculate new loadings
    for (g in 1:n_groups){
      subdata = data %>% filter(group==g)
      #skip changing things if there isn't any data
      if (nrow(subdata)==0){
        next
      }
      gmean[[g]] = as.numeric(colMeans(subdata[,variables]))
      gvar[[g]] = as.numeric(colwise(var)(subdata[,variables]))
    }
  }
  for (g in 1:n_groups){
    names(gmean[[g]]) = variables
    gvar[[g]] = diag(gvar[[g]])
    rownames(gvar[[g]]) = variables
    colnames(gvar[[g]]) = variables
  }
  return(list(classes=data$group, probs = probs, mu=gmean, sigma=gvar, priors=priors))
}

#test out
if (FALSE){
  test_df1 = data.frame(x=rnorm(50), y=rnorm(50), z=rnorm(50), group='a')
  test_df2 = data.frame(x = rnorm(50)*2 + 0.4, y=rnorm(50) - 0.1, z = rnorm(50)*10, group='b')
  test_df3 = data.frame(x=rnorm(50) - 2, y=rnorm(50), z=rnorm(50)/3 + 0.3, group='c')
  
  test_df = rbind(test_df1, test_df2, test_df3)
  
  test_gen = gen_model(test_df, c('x','y','z'), 3)
}

#quadratic model...also calculates correlations
quadratic_gen_model <- function(data, variables, n_groups, n_iter = 30, smooth_const = 0.02){
  priors = rep(1/n_groups, n_groups)
  initial_groups = sample(1:n_groups, nrow(data), replace=TRUE)
  data$group = initial_groups
  n_rows = nrow(data)
  gmean = list()
  gvar = list()
  for (g in 1:n_groups){
    subdata = data %>% filter(group==g)
    gmean[[g]] = as.numeric(colMeans(subdata[,variables]))
    gvar[[g]] = var(subdata[,variables])
  }
  #actual priors will not be set until during first iteration
  probs = matrix(nrow=nrow(data), ncol=n_groups)
  for (i in 1:n_iter){
    #calculate new probs
    for (g in 1:n_groups){
      probs[,g] = priors[g] * dmvnorm(data[,variables], mean=gmean[[g]], sigma = gvar[[g]])
      priors[g] = sum(data$group==g)/n_rows
    }
    #smoothing
    priors = (priors + smooth_const)/sum(priors + smooth_const)
    
    #reassign groups
    data$group = apply(probs, 1, which.max)
    
    #calculate new loadings
    for (g in 1:n_groups){
      subdata = data %>% filter(group==g)
      #skip changing things if there isn't any data
      if (nrow(subdata)==0){
        next
      }
      gmean[[g]] = as.numeric(colMeans(subdata[,variables]))
      gvar[[g]] = var(subdata[,variables])
    }
  }
  for (g in 1:n_groups){
    names(gmean[[g]]) = variables
    rownames(gvar[[g]]) = variables
    colnames(gvar[[g]]) = variables
  }
  return(list(classes=data$group, probs = probs, mu=gmean, sigma=gvar, priors=priors))
}