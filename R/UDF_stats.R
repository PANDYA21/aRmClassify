#### UDFs for statistical analysis

# ## get TP, FP, TN and FN
# getStats <- function(err.date, norm.df, threshold){
#   norm.df.train <- norm.df
#   err.date <- as.Date(err.date)
#   trend.date <- as.character(norm.df.train$Time[which(norm.df.train$x > threshold)])
#   prev.day <- ((as.Date(err.date)-1) %in% as.Date(trend.date))
#   this.day <- (as.Date(err.date) %in% as.Date(trend.date))
#   # TRUE pos
#   TP <- numeric(0)
#   if(all(this.day) | all(prev.day)){
#     TP <- length(err.date)
#   }else{
#     TP <- length(which(this.day | prev.day))
#   }
#   # FALSE pos
#   FP <- numeric(0)
#   rem.date <- trend.date[!(as.Date(trend.date) %in% as.Date(err.date))]
#   r2 <- rem.date[!(as.Date(rem.date) %in% (as.Date(err.date)-1))]
#   r3 <- r2[!(as.Date(r2) %in% (as.Date(err.date)-2))]
#   r4 <- r3[!(as.Date(r3) %in% (as.Date(err.date)-3))]
#   r5 <- r4[!(as.Date(r4) %in% (as.Date(err.date)+1))]
#   r6 <- r5[!(as.Date(r5) %in% (as.Date(err.date)+2))]
#   FP <- length(r6)
#   # FALSE neg
#   FN <- length(err.date) - TP
#   # TRUE neg
#   TN <- length(norm.df.train$x[norm.df.train$x < threshold]) - FN
#   # return(c("TP"=TP, "FP"=FP, "TN"=TN, "FN"=FN))
#   # return("TP"=TP)
#   return(c("TP"=TP, "FP"=FP, "TN"=TN, "FN"=FN))
# }

## get TP, FP, TN and FN (v2.0)
getStats <- function(err.date, norm.df, threshold, region=7){
  norm.df.train <- norm.df
  err.date <- as.Date(err.date)
  trend.date <- as.character(norm.df.train$Time[which(norm.df.train$x > threshold)])
  # prev.day <- ((as.Date(err.date)-1) %in% as.Date(trend.date))
  # this.day <- (as.Date(err.date) %in% as.Date(trend.date))
  if(length(err.date) == 0){
    TP = 0
    FP <- length(trend.date)
  } else {
    days <- lapply(0:region,
                   function(x){
                     return((as.Date(err.date)-x) %in% as.Date(trend.date))
                   })
    days <- data.frame(t(as.data.frame(days)))
    rownames(days) <- NULL
    res <- apply(days, 2, any)
    # TRUE pos
    TP <- length(which(res))
    # FALSE pos
    FP <- numeric(0)
    for(ii in 1:length(err.date))
    {
      trend.date <- trend.date[!as.Date(trend.date) %in% 
                                 as.Date((err.date[ii]-region):(err.date[ii]))]
    }
    FP <- length(trend.date)
  }
  # FALSE neg
  FN <- length(err.date) - TP
  # TRUE neg
  TN <- length(norm.df.train$x[norm.df.train$x < threshold]) - FN
  # return(c("TP"=TP, "FP"=FP, "TN"=TN, "FN"=FN))
  # return("TP"=TP)
  return(c("TP"=TP, "FP"=FP, "TN"=TN, "FN"=FN))
}


## get precision calculated
precision <- function(...){
  res <- data.frame(t(getStats(...)))
  return("precision" = res$TP[1]/(res$TP[1]+res$FP[1]))
}

## get FDR calculated
FDR <- function(...){
  return("FDR" = 1 - precision(...))
}

## get sensitivity calculated
sensitivity <- function(...){
  res <- data.frame(t(getStats(...)))
  return("precision" = res$TP[1]/(res$TP[1]+res$FN[1]))
}

## get FPR calculated
FPR <- function(...){
  res <- data.frame(t(getStats(...)))
  return("precision" = res$FP[1]/(res$FP[1]+res$TN[1]))
}



#### new method to calculate conf mat
###  see tp.81.x_x_x.R
confMat <- function(err.date, norm.df, threshold, 
                    leadTime = 0, monitorTime = 7, more.info = F){
  ## Alarm based confusion matrix
  ## Based on Bosch, USA
  
  err.date <- as.Date(unique(err.date))
  # predicted labels
  pred.label <- norm.df
  pred.label$x[pred.label$x >= threshold] <- 1
  pred.label$x[pred.label$x < threshold] <- 0
  # actual labels 
  true.label <- norm.df
  true.label$x[which(as.Date(true.label$Time) %in% as.Date(err.date))] <- 1
  true.label$x[which(!as.Date(true.label$Time) %in% as.Date(err.date))] <- 0
  # aux labels based on prediction horizon
  aux.label <- norm.df
  aux.label$x <- 0
  for(ed in err.date) {
    aux.label$x[which(as.Date(aux.label$Time) %in% as.Date((ed-monitorTime):(ed-leadTime)) )] <- 1
  }
  aux.label$x[which(aux.label$x != 1)] <- 0
  
  # get TP, FN, FP, TN
  TP <- length(which(which(aux.label$x == 1) %in%   which(pred.label$x == 1)))
  FN <- length(which(which(aux.label$x == 1) %in%   which(pred.label$x == 0)))
  FP <- length(which(which(aux.label$x == 0) %in%   which(pred.label$x == 1)))
  TN <- length(which(which(aux.label$x == 0) %in%   which(pred.label$x == 0)))
  # actual prediction horizon
  hor <- lapply(1:length(err.date),
                function(ii) {
                  ans <- max(which(true.label$x == 1)[ii] -
                               which(pred.label$x == 1), na.rm = T)
                  if(ans < 0) {
                    return(0)
                  } else {
                    return(ans)
                  }
                })
  mean.pred.hor <- mean(unlist(hor))
  
  # return
  if(more.info == F) {
    return(c("TP" = TP, "FN" = FN, "FP" = FP, "TN" = TN, 
             "mean.pred.hor" = mean.pred.hor))
  } else {
    return(list("TP" = TP, "FN" = FN, "FP" = FP, "TN" = TN, 
                "pred.hors" = unlist(hor),    
                "mean.pred.hor" = mean.pred.hor,
                "true.label" = true.label$x,
                "aux.label" = aux.label$x,
                "pred.label" = pred.label$x))
  }
}
### plot ROc based on confMat

# func to plot ROc
plotROC <- function(...) {
  ### Fucntion to plot ROC
  ### based on calculation of confusion matrix via confMat mathod
  thresholds <- seq(0.1,1,0.05)
  confs <- lapply(thresholds, 
                  function(thres) {
                    return(confMat(threshold = thres, ...))
                  })
  tprs <- unlist(lapply(confs, 
                        function(x) {
                          return(x[1] / (x[1] + x[2]))
                        }))
  fprs <- unlist(lapply(confs, 
                        function(x) {
                          return(x[3] / (x[3] + x[4]))
                        }))
  plot(x = fprs, y = tprs, main = "ROC curve", type = "l",
       xlab = "FPR", ylab = "TPR", lwd = 2)
  return()
}

