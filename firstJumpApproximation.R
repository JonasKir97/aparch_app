if(FALSE) {
  #generate levy  
  n <- 1e5
  normals <- rnorm(n)
  brownian <- cumsum(normals)
  TT <- 10
  
  ld <- simulateBrownianMotion(timeGrid = seq(0,TT,length.out = n), mu = 0, sigma = 1, gs = 0.001, randomSeed = 2021)
  
  #Szimayer Maller: pdf p.6: take N(n) = 2^n and dt_i(n) = T * 2^(-n) for i = 0,...,N(n)
  N <- 2^(1:10)
  tns0 <- lapply(N, function(n2) seq(0,TT,length.out = n2+1))
  mns0 <- seq(1,0,length.out = length(N)+1)[-(length(N)+1)]
  
  ff <- firstJumpApproximation(ld,tns0,mns0)
  
  ffd <- do.call("rbind",ff$proxiDataList)
  ffd$type <- paste0(ffd$intervalCount, " Intervalle, Sprunghöhe: ",ffd$m)
}

#' get the index of the first element in the vector \code{vec} which is geq val or return \code{-1} if such an element doesn't exist
firstGreaterEqualIndex <- function(vec,val) {
  ind <- which.max(vec>=val)
  if(val > vec[ind]) return(-1)
  return(ind)
}

#' calculate a first jump approximation
#' @param levyData the levyData with elements called \code{jumpTimes} and \code{levyProcess} containing the jump times and 
#' the values of the levyprocess, respectively, as given by the simulationfunctions simulateCompoundPoisson,simulateVarianceGamma or simulateBrownianMotion
#' @param tns the discretization times per step of the approximation
#' @param mns the jumpsizes to be crossed, must be same length as \code{tns}
#' @return a list with elements \code{proxiDataList} (a list of length \code{length(tns)} with the approximation dataframes) and
#' \code{fjaPlotList} (a list of plots showing the FJA at each step of the approximation)
firstJumpApproximation <- function(levyData, tns, mns) {
  
  if(length(tns) != length(mns)) stop("tns and mns must have same length")
  
  jumpTimes <- levyData[["jumpTimes"]]
  levyProcess <- levyData[["levyProcess"]]
  
  jumpSizes <- c(0,levyProcess[-1]-levyProcess[-length(levyProcess)])
  jumpSizesAbs <- abs(jumpSizes)
  
  #get the approximations as list of times and values (where each has the length of the interval specified in tns)
  print(length(tns))
  proxis <- lapply(1:length(tns), function(k) {
    tn <- tns[[k]]
    m <- mns[[k]]
    a <- Sys.time()
    prox <- sapply(2:length(tn), function(i) {
      timeIndsInCurrentIntervall <- jumpTimes >= tn[i-1] & jumpTimes < tn[i]
      currentAbsJumpSizes <- jumpSizesAbs[timeIndsInCurrentIntervall]
      firstJumpOverThresholdIndex <- firstGreaterEqualIndex(vec = currentAbsJumpSizes, val = m)
      if(firstJumpOverThresholdIndex == -1) return(NA)
      return(levyProcess[timeIndsInCurrentIntervall][firstJumpOverThresholdIndex])
    })
    print(paste0("Proxstep done in ", Sys.time()-a))
    list(t = tn[-length(tn)], prox = prox)
  })
  
  #generate list of plotdata 
  proxiDataList <- lapply(1:length(tns), function(k) {
    tn <- tns[[k]][-1]
    xStepFunData <- as.vector(rbind(c(0,tn[-length(tn)]),tn))
    yStepFunData <- as.vector(rbind(proxis[[k]]$prox,proxis[[k]]$prox))
    data.frame(x = xStepFunData, 
               y = yStepFunData, 
               intervalCount = rep(length(tn),length(xStepFunData)),
               m = rep(mns[[k]],length(xStepFunData)), stringsAsFactors = FALSE)
  })
  
  #generate list of plots
  fjaPlotList <- lapply(proxiDataList, function(pd) {
    basicLinePlot(x = pd$x, y = pd$y, plotTitle = paste0("Intervalle: ", pd$intervalCount[1],", Sprunghöhe: ",pd$m[1]), 
                  xLabel = "Zeit", yLabel = "First-Jump-Approximation", lineColor = "white") + 
      scale_x_continuous(limits = c(pd$x[1],pd$x[length(pd$x)]), expand = c(0,0))
  })
  
  list(
    proxiDataList = proxiDataList,
    fjaPlotList = fjaPlotList
  )
}