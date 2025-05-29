# Jags-Ydich-XnomSsubj-Mbernbeta.R 
# Accompanies the book:
#   Kruschke, J. K. (2014). Doing Bayesian Data Analysis: 
#   A Tutorial with R, JAGS, and Stan. 2nd Edition. Academic Press / Elsevier.
source("DBDA2E-utilities.R")
#===============================================================================

genMCMC = function( data , numSavedSteps=50000 , saveName=NULL ) { 
  require(rjags)
  #-----------------------------------------------------------------------------
  # THE DATA.
  # N.B.: This function expects the data to be a data frame, 
  # with one component named y being a vector of integer 0,1 values,
  # and one component named s being a factor of subject identifiers.
  y = data$y
  s = as.numeric(data$s) # converts character to consecutive integer levels
  # Do some checking that data make sense:
  if ( any( y!=0 & y!=1 ) ) { stop("All y values must be 0 or 1.") }
  Ntotal = length(y)
  Nsubj = length(unique(s))
  # Specify the data in a list, for later shipment to JAGS:
  dataList = list(
    y = y ,
    s = s ,
    Ntotal = Ntotal ,
    Nsubj = Nsubj
  )
  #-----------------------------------------------------------------------------
  # THE MODEL.
  modelString = "
  model {
    for ( i in 1:Ntotal ) {
      y[i] ~ dbern( theta[s[i]] ) #服从二项分布
    }
    for ( sIdx in 1:Nsubj ) {
      theta[sIdx] ~ dbeta( 2 , 2 ) # N.B.: 2,2 prior; change as appropriate.服从贝塔分布
    }
  }
  " # close quote for modelString
  writeLines( modelString , con="TEMPmodel.txt" ) #保存MCMC样本
  #-----------------------------------------------------------------------------
  # INTIALIZE THE CHAINS.
  # Initial values of MCMC chains based on data:
  # Option 1: Use single initial value for all chains:使用每个受试者的成功比例作为初始值。
  # thetaInit = rep(0, Nsubj)
  # for (sIdx in 1:Nsubj) { # 遍历每个受试者
  #   includeRows = (s == sIdx) # 找出属于当前受试者的行
  #   yThisSubj = y[includeRows]  # 提取当前受试者的所有投篮结果
  #   thetaInit[sIdx] = sum(yThisSubj) / length(yThisSubj) # 计算当前受试者的投篮命中率
  # }
  # initsList = list(theta = thetaInit)
  # Option 2: Use function that generates random values near MLE: #通过重采样生成初始值，并将其调整到 (0, 1) 范围内。
  initsList = function() {
    thetaInit = rep(0, Nsubj)
    for (sIdx in 1:Nsubj) { # 遍历每个受试者
      includeRows = (s == sIdx) # 找出属于当前受试者的行
      yThisSubj = y[includeRows]  # 提取当前受试者的所有投篮结果
      resampledY = sample(yThisSubj, replace = TRUE) # 从当前受试者的数据中进行有放回的重采样
      thetaInit[sIdx] = sum(resampledY) / length(resampledY) # 计算重采样后的投篮命中率
    }
    thetaInit = 0.001 + 0.998 * thetaInit # 将初始值调整到 (0.001, 0.999) 范围内
    return(list(theta = thetaInit)) # 返回包含初始值的列表
  }
  #-----------------------------------------------------------------------------
  # RUN THE CHAINS
  parameters = c( "theta")     # The parameters to be monitored 定义要监控的参数
  # 设置 MCMC 采样的参数
  adaptSteps = 500             # 适应步骤（调整采样器参数）
  burnInSteps = 500            # 烧入步骤（丢弃初始部分以确保收敛）
  nChains = 4                  # 运行的 MCMC 链的数量（至少需要 2 条用于诊断）
  thinSteps = 1                # 薄化步骤（每隔多少步保存一次样本）
  nIter = ceiling((numSavedSteps * thinSteps) / nChains)  # 每条链的总迭代次数
  # Create, initialize, and adapt the model:
  jagsModel = jags.model( "TEMPmodel.txt" , data=dataList , inits=initsList , 
                          n.chains=nChains , n.adapt=adaptSteps )
  # Burn-in:
  cat( "Burning in the MCMC chain...\n" )
  update( jagsModel , n.iter=burnInSteps )
  # The saved MCMC chain:
  cat( "Sampling final MCMC chain...\n" )
  codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                              n.iter=nIter , thin=thinSteps )
  # resulting codaSamples object has these indices: 
  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ] 如果提供了保存名称，则保存 MCMC 样本
  if ( !is.null(saveName) ) {
    save( codaSamples , file=paste(saveName,"Mcmc.Rdata",sep="") )
  }
  return( codaSamples )
} # end function

#===============================================================================

smryMCMC = function(  codaSamples , compVal=0.5 , rope=NULL , 
                      compValDiff=0.0 , ropeDiff=NULL , saveName=NULL ) {
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  Ntheta = length(grep("theta",colnames(mcmcMat)))
  summaryInfo = NULL
  rowIdx = 0
  for ( tIdx in 1:Ntheta ) {
    parName = paste0("theta[",tIdx,"]")
    summaryInfo = rbind( summaryInfo , 
      summarizePost( mcmcMat[,parName] , compVal=compVal , ROPE=rope ) )
    rowIdx = rowIdx+1
    rownames(summaryInfo)[rowIdx] = parName
  }
  for ( t1Idx in 1:(Ntheta-1) ) {
    for ( t2Idx in (t1Idx+1):Ntheta ) {
      parName1 = paste0("theta[",t1Idx,"]")
      parName2 = paste0("theta[",t2Idx,"]")
      summaryInfo = rbind( summaryInfo , 
        summarizePost( mcmcMat[,parName1]-mcmcMat[,parName2] ,
                       compVal=compValDiff , ROPE=ropeDiff ) )
      rowIdx = rowIdx+1
      rownames(summaryInfo)[rowIdx] = paste0(parName1,"-",parName2)
    }
  }
  if ( !is.null(saveName) ) {
    write.csv( summaryInfo , file=paste(saveName,"SummaryInfo.csv",sep="") )
  }
  show( summaryInfo )
  return( summaryInfo )
}

#===============================================================================

plotMCMC = function( codaSamples , data , compVal=0.5 , rope=NULL , 
                     compValDiff=0.0 , ropeDiff=NULL , 
                     saveName=NULL , saveType="jpg" ) {
  #-----------------------------------------------------------------------------
  # N.B.: This function expects the data to be a data frame, 
  # with one component named y being a vector of integer 0,1 values,
  # and one component named s being a factor of subject identifiers.
  y = data$y
  s = as.numeric(data$s) # converts character to consecutive integer levels
  # Now plot the posterior:
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  chainLength = NROW( mcmcMat )
  Ntheta = length(grep("theta",colnames(mcmcMat)))
  openGraph(width=2.5*Ntheta,height=2.0*Ntheta)
  par( mfrow=c(Ntheta,Ntheta) )
  for ( t1Idx in 1:(Ntheta) ) {
    for ( t2Idx in (1):Ntheta ) {
      parName1 = paste0("theta[",t1Idx,"]")
      parName2 = paste0("theta[",t2Idx,"]")
      if ( t1Idx > t2Idx) {  
        # plot.new() # empty plot, advance to next
        par( mar=c(3.5,3.5,1,1) , mgp=c(2.0,0.7,0) )
        nToPlot = 700
        ptIdx = round(seq(1,chainLength,length=nToPlot))
        plot ( mcmcMat[ptIdx,parName2] , mcmcMat[ptIdx,parName1] , cex.lab=1.75 ,
               xlab=parName2 , ylab=parName1 , col="skyblue" )
      } else if ( t1Idx == t2Idx ) {
        par( mar=c(3.5,1,1,1) , mgp=c(2.0,0.7,0) )
        postInfo = plotPost( mcmcMat[,parName1] , cex.lab = 1.75 , 
                             compVal=compVal , ROPE=rope , cex.main=1.5 ,
                             xlab=parName1 , main="" )
        includeRows = ( s == t1Idx ) # identify rows of this subject in data
        dataPropor = sum(y[includeRows])/sum(includeRows) 
        points( dataPropor , 0 , pch="+" , col="red" , cex=3 )
      } else if ( t1Idx < t2Idx ) {
        par( mar=c(3.5,1,1,1) , mgp=c(2.0,0.7,0) )
        postInfo = plotPost(mcmcMat[,parName1]-mcmcMat[,parName2] , cex.lab = 1.75 , 
                           compVal=compValDiff , ROPE=ropeDiff , cex.main=1.5 ,
                           xlab=paste0(parName1,"-",parName2) , main="" )
        includeRows1 = ( s == t1Idx ) # identify rows of this subject in data
        dataPropor1 = sum(y[includeRows1])/sum(includeRows1) 
        includeRows2 = ( s == t2Idx ) # identify rows of this subject in data
        dataPropor2 = sum(y[includeRows2])/sum(includeRows2) 
        points( dataPropor1-dataPropor2 , 0 , pch="+" , col="red" , cex=3 )
      }
    }
  }
  #-----------------------------------------------------------------------------  
  if ( !is.null(saveName) ) {
    saveGraph( file=paste(saveName,"Post",sep=""), type=saveType)
  }
}

#===============================================================================
