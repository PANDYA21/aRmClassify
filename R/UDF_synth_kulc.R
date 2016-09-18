## a function to generate association rules and kulc measure
## returns a list of data.frame object, rules, transactions and itemMatrix
getRules <- function(Vect, minLen = 2, maxLen = 2,
                     Width, minSup = 1e-3, minConf = 1e-8,
                     interestMeasureString = "kulc",
                     method.rule = "roll"){
  rules <- getArulesGen(
    vect = Vect,
    minlen = minLen,
    maxlen = maxLen,
    width = Width,
    minsup = minSup,
    minconf = minConf
  )
  kulcs <- interestMeasure(
    x = rules$rules,
    measure = interestMeasureString,
    transactions = rules$tran,
    reuse = TRUE
  )
  rulesDF <- as(rules$rules, "data.frame")
  rulesDF$kulcs <- kulcs
  rulesDF$relativeKulc <- kulcs/min(kulcs)
  rulesDF$kulcMenaPerc <- kulcs/mean(kulcs)

  return(
    list(
      df = rulesDF[order(rulesDF$kulcs, decreasing = TRUE), ],
      rules = rules$rules,
      tran = rules$tran,
      itemMat = rules$itemMatrix
    )
  )
}


### break a vector to a list by dividing ata specific itme
breakList <- function(vect, breakItem){
  ind <- c(0, which(vect == breakItem), length(vect))
  return(lapply(1:(length(ind)-1), function(ii) vect[(ind[ii]+1):(ind[ii+1])]))
}


### min-max normalization
minMaxNorm <- function(vect, ...){
  vect <- c(0, vect)
  return( (vect - min(vect, na.rm = TRUE)) / (max(vect, na.rm = TRUE) - min(vect, na.rm = TRUE)) )
}


