#' helper to validate the inputs for the discrete simulation of an APARCH(1,1)-process
validateAndProcessDiscreteSimulationInput <- function(shinyInputObject) {
  deltas <- shinyInputObject$deltaDiscrete #Zeichenkette, ggf kommagetrennt für mehrere Simulationen mit variierenden Delta
  deltas <- as.numeric(strsplit(deltas, ",")[[1]])
  if(any(is.na(deltas)) || any(deltas <=0)) {#Konvertierung in Numerisch ging an mindestens einer Stelle schief, Fehler ausgeben
    return(list(errorText = "Ungültige Eingabe in den Deltas (Erwarte: Kommagetrennt,Punkt als Dezimalzeichen, größer 0)"))
  }
  
  gammas <- shinyInputObject$gammaDiscrete #Zeichenkette, ggf kommagetrennt für mehrere Simulationen mit variierenden Gamma
  gammas <- as.numeric(strsplit(gammas, ",")[[1]])
  if(any(is.na(gammas)) || any(abs(gammas)>=1)) {
    return(list(errorText = "Ungültige Eingabe in den Gammas (Erwarte: Kommagetrennt,Punkt als Dezimalzeichen, betragsmäßig kleiner 1)"))
  }
  
  theta <- as.numeric(shinyInputObject$thetaDiscrete)
  if(is.na(theta)) {
    return(list(errorText = "Ungültige Eingabe im Theta"))
  }
  
  alpha <- as.numeric(shinyInputObject$alphaDiscrete)
  if(is.na(alpha)) {
    return(list(errorText = "Ungültige Eingabe im Alpha"))
  }
  beta <- as.numeric(shinyInputObject$betaDiscrete)
  if(is.na(beta)) {
    return(list(errorText = "Ungültige Eingabe im Beta"))
  }
  
  steps <- as.integer(shinyInputObject$simulationStepsDiscrete)
  if(is.na(steps)) {
    return(list(errorText = "Ungültige Eingabe in der Länge der Simulation"))
  }
  
  return(
    list(errorText = NULL,
         deltaVec = deltas,
         gammaVec = gammas,
         theta = theta,
         alpha = alpha,
         beta = beta,
         steps = steps)
  )
  
}

#' the function h occuring in the definition of an APARCH(1,1)-process
h <- function(x,gamma,delta) {
  return((abs(x)-gamma*x)^delta)
}


#' function to simulate a discrete time APARCH(1,1) process
#' 
simulateDiscreteAPARCH11 <- function(steps = 1000,
                                     alpha = 0.5,
                                     beta = 0.3,
                                     theta = 0.5,
                                     gamma = 0.5,
                                     delta = 2, 
                                     noiseGenerator = function(n) {return(rnorm(n, mean = 0, sd = 1))},
                                     fixedNoises = NULL,
                                     useCPP = TRUE) {
  
  if(!is.null(fixedNoises)) {
    epsilons <- fixedNoises
  } else {
    epsilons <- noiseGenerator(steps)
  }
  
  if(useCPP) {
    resCpp <- simulateDiscreteAPARCH11inCPP(noises = epsilons, alpha = alpha, beta = beta, theta = theta, gamma = gamma, 
                                            delta = delta, initialSigmaDelta = 0, initialY = 0)
    sigmaDelta <- resCpp$sigmaDelta
    Y <- resCpp$Y
  } else {
    
    hFixed <- function(x) {return(h(x = x, gamma = gamma, delta = delta))}
    oneOverDelta <- 1/delta
    
    res <-  do.call("rbind", Reduce(
      f = function(sigAndY, newNoise) {
        newSigDelta <- theta + alpha*hFixed(sigAndY[2]) + beta * sigAndY[1]
        c(newSigDelta, newNoise * newSigDelta^oneOverDelta)
      },
      x = epsilons, 
      init = c(0,0), #sigma^delta and Y
      accumulate = TRUE
    ))
    sigmaDelta <- res[,1]
    Y <- res[,2]
  }
  
  return(list(noises = epsilons, sigmaDelta = sigmaDelta, Y = Y))
}

#' simulate a compound Poisson process as driving Levy process on a given timegrid with intensity lambda
#' @param timeGrid the timegrid given as vector
#' @param lambda the intensity of the exponential distribution used to simulate the interarrivaltimes
#' @param levyJumpGenerator a named list consisting of 
#' \code{FUN} : a function that is called to generate random variables 
#' \code{namedArgs} : a named list consisting of the named arguments with values for the function given in \code{FUN}
#' \code{countArgName} : the name of the argument of \code{FUN} which identifies the number of random variables that should be simulated
#' defaults to a normal distribution with a mean of 0 and a standard deviation of 1
#' @param randomSeed an integer specifying a seed for reproducibility
#' @return a named list consisting of
#' \code{jumpTimes} : a vector of the processes jump times
#' \code{levyJumps} : a vector with the jumps of the Levy process
#' \code{levyProcess} : a vector with the values of the Levy process (compound Poisson process)
simulateCompoundPoisson <- function(timeGrid = 1:10, 
                                    lambda = 1,
                                    levyJumpGenerator = list(FUN = stats::rnorm,
                                                             namedArgs = list(mean = 0, sd = 1),
                                                             countArgName = "n"),
                                    randomSeed = 2021) {
  
  set.seed(randomSeed)
  lastTimeToReach <- timeGrid[length(timeGrid)] #last time in the process, simulate exp-rvs until reached
  interarrivalTimes <- numeric(0)
  reachedLastTime <- timeGrid[1]
  
  while(reachedLastTime < lastTimeToReach) {
    remainingTime <- lastTimeToReach - reachedLastTime
    estimatedNeededValues <- ceiling(remainingTime + 3*sqrt(remainingTime)) #roughly so many values need to be generated to reach the last time
    newInterarrivalTimes <- stats::rexp(n = estimatedNeededValues, rate = lambda)
    interarrivalTimes <- c(interarrivalTimes,newInterarrivalTimes)
    reachedLastTime <- reachedLastTime + sum(newInterarrivalTimes)
  }
  
  jumpTimes <- cumsum(interarrivalTimes)
  jumpTimes <- jumpTimes[jumpTimes <= lastTimeToReach]
  
  jumpGenerator <- levyJumpGenerator[["FUN"]]
  
  neededArguments <- names(formals(jumpGenerator))
  suppliedArgumentNames <- c(names(levyJumpGenerator$namedArgs),levyJumpGenerator$countArgName)
  if(!all(neededArguments %in% suppliedArgumentNames)) stop("Missing arguments for FUN in levyJumpGenerator")
  
  n <- length(jumpTimes)
  argumentList <- c(levyJumpGenerator$namedArgs,setNames(list(n),levyJumpGenerator$countArgName))
  levyJumps <- do.call(jumpGenerator,argumentList)
  levyProcess <- cumsum(levyJumps)
  
  return(list(jumpTimes = jumpTimes,levyJumps = levyJumps, levyProcess = levyProcess))
}

#' simulate a Variance gamma process as driving Levy process
#' @param timeGrid the timegrid given as vector
#' @param sigma 
#' @param nu
#' @param theta
#' @param gs
#' @param randomSeed an integer specifying a seed for reproducibility
#' @return a named list consisting of
#' \code{jumpTimes} : a vector of the processes jump times
#' \code{levyProcess} : a vector with the values of the Levy process (Variance gamma process)
simulateVarianceGamma <- function(timeGrid = 1:10, 
                                  sigma = 1, 
                                  nu = 0.05, 
                                  theta = 0.5, 
                                  gs = 0.01, 
                                  randomSeed = 2021) {
  
  ts <- seq(0,timeGrid[length(timeGrid)],gs)
  dts <- ts[2:length(ts)]-ts[1:(length(ts)-1)]
  
  set.seed(randomSeed)
  gammaVariables <- stats::rgamma(n = length(dts), shape=(1/nu)*dts, scale=nu)
  normalsForBrownian <- stats::rnorm(n = length(dts), mean = 0, sd = sqrt(gammaVariables))
  brownian <- c(0,cumsum(normalsForBrownian))
  varianceGammaProcess <- theta*c(0,cumsum(gammaVariables))+sigma*brownian
  
  return(list(jumpTimes = ts, levyProcess = varianceGammaProcess))
}
