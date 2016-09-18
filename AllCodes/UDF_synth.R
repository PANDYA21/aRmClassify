#############################################################################
## Synthetic data generation to evaluate ARM on sequential data - UDFs
#############################################################################
## Bhaumik Pandya
## Master student - Automation and IT
## Fachhochschule Koeln, Abteilung Gummersbach
## Praktikant, Bosch TT/ESY2, Lollar
#############################################################################

udf.dir <- "D:/State_transition/Codes/Final/"
# width = 50 # window size to create transactions
# len = 4 # length of lhs of the rules.

# setwd("D:/State_transition/Codes/Final/")
# source("UDF.R")
# source("UDF_SS.R")
# source("UDF_SS_part2.R")
# source("UDF_par.R")
# source("UDF_stats.R")
# source("UDF_ARM.R")
library(arules)
library(zoo)
library(xts)
library(data.table)
library(ggplot2)
library(tikzDevice)
library(xts)
require("parallel")
require("foreach")
require("doParallel")
require("doSNOW")


# UDFs --------------------------------------------------------------------
### support, confidence and lift
suppn <- function(x, vect, width=1){
  library(zoo)
  dat <- data.frame(rollapply(vect, width, rbind))
  res <- apply(dat, 1, function(xx){return(all(x %in% xx))})
  return(length(which(res))/length(res))
}
confidencen <- function(x, y, vect, ...){
  return(suppn(c(x,y), vect, ...)/suppn(x, vect, ...))
}
liftn <- function(x, y, vect, ...){
  return(confidencen(x, y, vect, ...)/suppn(y, vect, ...))
}

{
# ### plot funcs
# plotTikzStart <- function(path = getwd(), nam = "tikzPlot", 
#                           width = 10, height = 6){
#   tf <- file.path(path, strcat(c(nam, ".tex")))
#   tikz(tf,standAlone=TRUE, width = width, height = height)
#   return(tf)
# }
# plotTikzEnd <- function(tf, compile.pdf = TRUE, open.pdf = TRUE){
#   dev.off()
#   if(compile.pdf == TRUE){
#     ## generate .pdf plot
#     tools::texi2dvi(tf,pdf=T,clean=T)
#     # # View the output
#     if(open.pdf == TRUE){
#       system(paste(getOption('pdfviewer'),gsub(".tex", ".pdf", tf)))
#     }
#   }
# }
}

### the arules func - generelized
getArulesGen <- function(vect, minlen=4, maxlen = 4, 
                         width, minsup, minconf){
  bad.seq <- "error"
  seq.rle <- vect
  freq.pat <- as.character(unique(seq.rle))
  
  seq.vec <- c(as.character(seq.rle))
  seq.mat <- data.table(rollapply(seq.vec, width, rbind))
  seq.mat <- eliminateAfterErrorSeqs(seq.mat)
  seq.clus.bin <- apply(seq.mat, 1, 
                        function(x){
                          xx <- freq.pat %in% as.character(x)
                          return(xx)
                        })
  seq.clus.bin <- data.frame(t(data.frame(seq.clus.bin)))
  names(seq.clus.bin) <- freq.pat
  rownames(seq.clus.bin) <- NULL
  seq.clus.bin <- sapply(seq.clus.bin, as.logical)
  
  ## apriori algorithm
  i <- as(seq.clus.bin, "itemMatrix")
  tran <- as(seq.clus.bin, "transactions")
  
  rules <- apriori(tran,
                   parameter = list(support = minsup, confidence = minconf, 
                                    minlen=minlen, maxlen=maxlen),
                   appearance = list(rhs = c(bad.seq), default="lhs"))
  return(rules)
}

## generate random sequence - parameter based
randSeq.old <- function(region = c(50,70), max.inc = 5.4){
  ## 15 sequences (for e.g.)
  labs <- unlist(lapply(1:15, function(x){paste0("S",x)}))
  ## range of higher probability
  lowlim <- region[1]
  uplim <- region[2]
  ## Higher probabilitoes
  prob.tan <- tan(seq(atan(1), atan(max.inc), 0.01))
  ## normal probability of all states (equal)
  normprob <- 1/length(labs)
  ## random,location for error
  loc <- sample(c(400:700), 1)
  ## equal prob (mean zero) distribution until location
  rand <- sample(labs, size = loc, 
                 replace = T,
                 prob = rep(normprob, length(labs)))
  ## higher probablity region with "tan" distribution
  chunks <- length(prob.tan)
  loc2 <- sample(c(lowlim:uplim), 1)
  rand.er <- unlist(lapply(1:chunks, 
                           function(ii){
                             sample(labs, size = round(loc2/chunks), 
                                    replace = T,
                                    prob = c(rep(normprob*prob.tan[ii], 3), 
                                             rep(normprob, length(labs)-3)))
                           }))
  ## normal sequences again
  rand3 <- sample(labs, size = sample(50:100, 1, T), 
                  replace = T,
                  prob = rep(normprob, length(labs)))
  ## combined sequence
  Seq <- c(rand, rand.er, "error", rand3)
  return(Seq)
}

## generate random sequence - default
randSeqGen.old <- function(len = 10000, moreinfo = F, 
                       plot.prob = T, max.inc = 5.4, 
                       region = c(70,90), n.err, ...){
  if(missing(n.err)){
    cou <- round(len/685)
  } else {
    cou <- n.err
  }
  uplim <- region[2]
  ## get the seq
  Seq <- c(unlist(lapply(1:cou, 
                         function(ii){
                           return(randSeq(max.inc = max.inc,
                                          region = region, ...))
                         })
  )
  )
  ## plot the curve if wnated
  if(plot.prob == T){
    ## Higher probabilitoes
    prob.tan <- tan(seq(atan(1), atan(max.inc), 0.01))
    plot(prob.tan, type = "b", xlab = "",
         main = "Probability distribution of chosen sequences just before error",
         xaxt = "n", ylab = "Multiplier to the normal probability")
  }
  ## return as desired
  if(moreinfo == T){
    loc.err <- which(Seq == "error")
    prob.s1 <- unlist(apply(cbind((loc.err-uplim), loc.err), 1,
                            function(x){ suppn("S1", Seq[x[1]:x[2]]) }))
    prob.s2 <- unlist(apply(cbind((loc.err-uplim), loc.err), 1,
                            function(x){ suppn("S2", Seq[x[1]:x[2]]) }))
    prob.s3 <- unlist(apply(cbind((loc.err-uplim), loc.err), 1,
                            function(x){ suppn("S3", Seq[x[1]:x[2]]) }))
    res.list <- list("Seq" = Seq,  
                     "prob.s1" = prob.s1, 
                     "prob.s2" = prob.s2, 
                     "prob.s3" = prob.s3)
    return(res.list)
  }else{
    return(Seq)
  }
}


## Incrementing probability distribution
getIncProb <- function(region = 50, max.inc = 10, min.prob = 1/30,
                       distr = c("sine", "cosine", 
                                "tangent", "expo1", "expo2")){
  ## Generates a sequence for increasing probabilities
  ## based on the input parameters. 
  if(missing(distr)){
    dist = "tanegent"
  } else {
    return(switch(distr,
           sine = sin(seq(min.prob, asin(1), 
                          (asin(1)-min.prob)/region ))*max.inc,
           cosine = max.inc-cos(seq(min.prob, acos(0),
                                    (acos(0)-min.prob)/region))*max.inc,
           tangent = tan(seq(min.prob, atan(10),
                             (atan(10)-min.prob)/region))*max.inc/10,
           expo1 = exp(seq(min.prob, log(max.inc), 
                           (log(max.inc)-min.prob)/region)),
           expo2 = (1 - exp(-seq(min.prob, log(max.inc),
                                 (log(max.inc)-min.prob)/region)))*max.inc))
    # plot(sin(seq(min.prob, asin(1), (asin(1)-min.prob)/region ))*max.inc)
  }
}

## generate random sequence
genSeq <- function(uniqs = 30, region = 100, min.prob = 1/uniqs,
                   max.inc = 5, len = region*5, distr){
  ## Generates a sequence of said unique sequences with "error" at
  ## the end. i.e. one chunk before an error.
  seqs <- sapply(1:uniqs, function(xx) return(strcat(c("S", xx))))
  norm.seqs <- sample(x = seqs, size = len-region, 
                      replace = T, prob = rep(min.prob, uniqs))
  ## the chunk before error
  highProbs <- getIncProb(region = round(region/10), 
                          max.inc, min.prob, distr)
  highProbs[which(highProbs < 1)] <- 1
  nchunks <- length(highProbs)
  loc.er <- region
  er.seqs <- unlist(lapply(1:nchunks, 
                           function(ii){
                             lp <- 1/(27+(3*highProbs[ii]))
                             hp <- lp*highProbs[ii]
                             sample(seqs, size = round(loc.er/nchunks), 
                                    replace = T,
                                    prob = c(rep(hp, 3), 
                                             rep(lp, length(seqs)-3)))
                           }))
  ## Generate random sequencese for seven chunks after error to
  ## make the support of the desired rule low i.e. like the -
  ## real world data.
  norm.seqs1 <- sample(x = seqs, size = 7*len-region, 
                      replace = T, prob = rep(min.prob, uniqs))
  ## return the sequential portion
  return(c(norm.seqs, er.seqs, "error", norm.seqs1))
}

## default function wrapper
genRandSeq <- function(slen = 80000, reg = 100, 
                       ners = round(slen*0.0007/8), ...){
  ## Generates a sequence with some errors in it, with increasing
  ## probabilitoes of S1, S2 and S3 before an error by supplied distr.
  flen <- slen/8
  seqs <- lapply(1:ners,
                 function(nn){
                   lowlim <- round(flen/ners*0.5)
                   upplim <- round(flen/ners*1.5)
                   len1 <- sample(x = c(lowlim:upplim), size = 1)
                   return(genSeq(region = reg, len = len1, ...))
                 })
  return(unlist(seqs))
}


### EoC ###