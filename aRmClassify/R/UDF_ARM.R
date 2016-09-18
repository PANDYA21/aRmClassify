#############################################################################
## User-Defined-Functions for Association Rule Mining (ARM)
## Master Thesis: Bhaumik Pandya
#############################################################################


#### get rules via arules package
getArules <- function(seqs.filt, width, minsup, minconf, len, minsup2=minsup/10){
  #### Execute apriori algorithm on sequences
  library(zoo)
  library(arules)
  library(data.table)
  
  loc <- which(seqs.filt$erc > 0)
  loc.rep <- loc[which(c(0, diff(loc)) == 1)]
  if(length(loc.rep) != 0){
    seqs.filt <- seqs.filt[-loc.rep]
  }
  
  #### make error sequences identical
  bad.seq <- "error"
  seqs.filt$listo[which( seqs.filt$erc > 0 )] <- bad.seq
  
  #### mask the sequences just before error (3 hrs before)
  loc <- which(seqs.filt$erc > 0)
  for(ll in loc)
  {
    seqs.filt$listo[which(seqs.filt$dat > seqs.filt$dat[ll] - 3600*3 &
                            seqs.filt$dat < seqs.filt$dat[ll])] <- "0"
  }
  
  seq.rle <- seqs.filt$listo
  freq.pat <- as.character(unique(seq.rle))
  freq.pat <- (freq.pat[!freq.pat %in% "0"])
  if(!bad.seq %in% freq.pat){
    freq.pat <- c(freq.pat, bad.seq)
  }
  if(!"failed" %in% freq.pat){
    freq.pat <- c(freq.pat, "failed")
  }
  
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
                                    minlen=len, maxlen=len),
                   appearance = list(rhs = c(bad.seq), default="lhs"))
  if(length(rules) == 0){
    cat("\n Reducing minimum support. \n")
    rules <- apriori(tran,
                     parameter = list(support = minsup2, confidence = minconf, 
                                      minlen=len, maxlen=len),
                     appearance = list(rhs = c(bad.seq), default="lhs"))
  }
  if(length(rules) == 0){
    cat("\n Reducing minimum support. \n")
    rules <- apriori(tran,
                     parameter = list(support = minsup2/2, confidence = minconf, 
                                      minlen=len, maxlen=len),
                     appearance = list(rhs = c(bad.seq), default="lhs"))
  }
  return(rules)
}


#### v2-0 getArules2
getArules2 <- function(seqs.filt, width, minsup, minconf, len, minsup2=minsup/2,
         mask3hrs = F, uniq.freq = F){
  #### Execute apriori algorithm on sequences
  library(zoo)
  library(arules)
  library(data.table)
  
  #### make error sequences identical
  bad.seq <- "error"
  seqs.filt$listo[which( seqs.filt$erc > 0 )] <- bad.seq
  
  if(mask3hrs == T){
    #### mask the sequences just before error (3 hrs before)
    loc <- which(seqs.filt$erc > 0)
    for(ll in loc)
    {
      seqs.filt$listo[which(seqs.filt$dat > seqs.filt$dat[ll] - 3600*3 &
                              seqs.filt$dat < seqs.filt$dat[ll])] <- "0"
    }
  }
  
  seq.rle <- seqs.filt$listo
  if(uniq.freq == T){
    freq.pat <- as.character(unique(seq.rle))
  } else {
    tab <- data.frame(table(seq.rle))
    freq.pat <- as.character(tab[ tab$Freq > 1, 1])
  }
  freq.pat <- (freq.pat[!freq.pat %in% "0"])
  freq.pat <- (freq.pat[!freq.pat %in% "good"])
  if(!bad.seq %in% freq.pat){
    freq.pat <- c(freq.pat, bad.seq)
  }
  if(!"failed" %in% freq.pat){
    freq.pat <- c(freq.pat, "failed")
  }
  
  seq.vec <- c(as.character(seq.rle))
  seq.mat <- data.frame(rollapply(seq.vec, width, rbind))
  seq.mat <- data.table(seq.mat)
  seq.mat <- eliminateAfterErrorSeqs(seq.mat)
  seq.mat <- data.frame(seq.mat)
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
                                    minlen = len, maxlen = len),
                   appearance = list(rhs = c(bad.seq), default="lhs"))
  if(length(rules) == 0){
    cat("\n Reducing minimum support. \n")
    rules <- apriori(tran,
                     parameter = list(support = minsup2, confidence = minconf, 
                                      minlen=len, maxlen=len),
                     appearance = list(rhs = c(bad.seq), default="lhs"))
  }
  if(length(rules) == 0){
    cat("\n Reducing Further minimum support. \n")
    rules <- apriori(tran,
                     parameter = list(support = minsup2/2, confidence = minconf, 
                                      minlen=len, maxlen=len),
                     appearance = list(rhs = c(bad.seq), default="lhs"))
  }
  return(rules)
}


#### Elimintae the sequences after error in the chunk (mask them)
eliminateAfterErrorSeqs <- function(seq.mat, ...){
  seq.mat <- data.table(seq.mat)
  inde <- apply(seq.mat, 1, 
                function(x){
                  wh <- which(x == "error")
                  if(length(wh) == 0){
                    return(0)
                  } else if(length(wh) > 1) {
                    return(-1)
                  } else {
                    return(wh)
                  }
                })
  inde <- unlist(inde)
  
  wh <- which(inde != 0 & inde != -1)
  ncols <- dim(seq.mat)[2]
  er.inde <- wh[which(inde[wh] != ncols)]
  er.loc <- inde[er.inde]
  for(ii in 1:length(er.inde))
  {
    seq.mat[er.inde[ii], c((er.loc[ii]+1):ncols):= "0"]
  }
  return(seq.mat)
}


#### label the chinks and weight them based on lift of the rule found
getChunkLabLift <- function(seqs.filt, width, lhss, rules.filt.df, plot.it){
  library(zoo)
  if(missing(plot.it)){
    plot.it = TRUE
  }
  cat("\nThe lengthy loop started\n")
  chunk.lab <- lapply(c(1:length(lhss)), 
                      function(ii){
                        data.table(t(data.table(rollapply(seqs.filt$listo, width, 
                          function(x){
                            if(all(lhss[[ii]] %in% x)){
                              return(rules.filt.df$lift[ii]*rules.filt.df$support[ii])
                            }else{
                              return(0)
                            }
                          }))))
                      })
  chunk.lab <- rbindlist(chunk.lab)
  chunk.lab <- apply(chunk.lab, 2, sum)
  
  seq.ord.lab <- c(rep(0, (width-1)), chunk.lab)
  er.seq <- rep(0, length(seqs.filt$listo))
  er.seq[which(seqs.filt$erc != 0)] <- 1000
  
  if(plot.it == FALSE){
    return(list("seq.ord.lab" = seq.ord.lab, "er.seq" = er.seq))
  }else{
    main.lab <- strcat(c("Gateway: ", str2num(seqs.filt$gateway[1])))
    plot(seq.ord.lab, type = "l", lwd = 2, xlab = "Sequence index", # ylim=c(0,1.2),
         ylab = "Measure of badness in sequences", main = main.lab)
    lines(er.seq, col="RED", lwd = 1.5)
    return(list("seq.ord.lab" = seq.ord.lab, "er.seq" = er.seq))
  }
}


#### get lhs of the rules as a list
getLhss <- function(rules.filt.df){
  lhss <- lapply(rules.filt.df$rules, 
                 function(x) unlist(strsplit(as.character(x), "=>") ))
  lhss <- unlist(lhss)
  lhss <- lhss[seq(1,length(lhss),by = 2)]
  lhss <- lapply(lhss, function(x) unlist(strsplit(x, "\\{")))
  lhss <- lapply(lhss, function(x) unlist(strsplit(x, "\\}")))
  lhss <- c(unlist(lhss))
  lhss <- lhss[seq(1,length(lhss),by = 2)]
  lhss <- lapply(lhss, function(x) unlist(strsplit(as.character(x), ",")))
  return(lhss)
}


#### a very useful function to plot using many ggplot2
###  1. direct plot via ggplot2
###  2. tikzDevice package .tex and .pdf compilation (for font = "tex" option only)
###  3. different font support ("ComputerModern" incl.)
plotAggGgTikz <- function(seqs.lab, f, agg.factor="day", font="default", tz="GMT", path=getwd(), 
                          name.suffix=NULL, gen.tex=T, compile.pdf=T, font.size=10, 
                          width=10, height=6){
  library(xts)
  library(ggplot2)
  seq.lab.ts <- make.ts2(x = as.numeric(seqs.lab$listo),
                         f = f, pack = "xts", 
                         ord = as.POSIXct(seqs.lab$dat, tz=tz), tz=tz)
  seq.lab.agg <- aggTs(x = seq.lab.ts, br.in = agg.factor, func = "sum")
  seq.lab.agg <- make.ts2(x = seq.lab.agg$x, f = f, pack = "xts", 
                          ord = as.POSIXct(seq.lab.agg$Time, tz=tz), tz = tz)
  
  seq.er.ts <- make.ts2(seqs.lab$erc, f, "xts", seqs.lab$dat, "GMT")
  seq.er.agg <- aggTs(seq.er.ts, agg.factor, sum)
  seq.er.agg <- make.ts2(seq.er.agg$x, f, "xts", 
                         as.POSIXct(seq.er.agg$Time, tz), tz)
  seq.er.agg[which(seq.er.agg != 0)] <- 1
  
  norm.df <- data.frame("Time" = index(seq.lab.agg), "x" = seq.lab.agg)
  row.names(norm.df) <- NULL
  
  ## location of errors
  loc.err <- which(seq.er.agg > 0)
  ii <- 1
  rects <- numeric(0)
  for(ll in loc.err)
  {
    xmin <- index(seq.er.agg[ll])
    xmax <- xmin+1800
    rect <- data.frame(xmin=as.POSIXct(xmin),
                       xmax=as.POSIXct(xmax), 
                       ymin=-Inf, ymax=Inf)
    rects <- rbind(rects, rect)
    ii <- ii+1
  }
  
  gg <- ggplot(norm.df, aes(Time, x)) + geom_line(size = 1.2) +
    xlab("Time") + ylab("Measure of badness in sequences") + #ylim(0,1) +
    ggtitle(strcat(c("Gateway: ", str2num(seqs.lab$gateway[1])))) +
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  
  oldwd <- getwd()
  setwd(path)
  
  if(font %in% c("tex", "TEX", "Tex", "LaTex", "pdftex", "pdfTex", "latex")){
    library(tikzDevice)
    if(gen.tex == TRUE){
      ## generate .tex file
      tf <- file.path(path, strcat(c(str2num(seqs.lab$gateway[1]), name.suffix, ".tex")))
      tikz(tf,standAlone=TRUE, width = width, height = height)
      print(gg + geom_rect(data=rects, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                           fill = "RED",
                           color="RED",
                           size = 1.1,
                           inherit.aes = FALSE)+ 
              theme_bw() + 
              theme(plot.title = element_text(face="bold", size=font.size)))
      dev.off()
      
      if(compile.pdf == TRUE){
        ## generate .pdf plot
        tools::texi2dvi(tf,pdf=T)
        # # View the output
        # system(paste(getOption('pdfviewer'),file.path(td,'example1.pdf')))
      }
    } else {
      pdf(file = strcat(c(str2num(seqs.lab$gateway[1]), name.suffix, ".pdf")), 
          width = width, height = height)
      print(gg + geom_rect(data=rects, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                           fill = "RED",
                           color="RED",
                           size = 1.1,
                           inherit.aes = FALSE)+ 
              theme_bw() + 
              theme(plot.title = element_text(face="bold", size=font.size)))
      dev.off()
    }
  } else if(font %in% c("default", "DEFAULT", "Default")) {
    pdf(file = strcat(c(str2num(seqs.lab$gateway[1]), name.suffix, ".pdf")), 
        width = width, height = height)
    print(gg + geom_rect(data=rects, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                         fill = "RED",
                         color="RED",
                         size = 1.1,
                         inherit.aes = FALSE)+ 
            theme_bw() + 
            theme(plot.title = element_text(face="bold", size=font.size)))
    dev.off()
  } else {
    pdf(file = strcat(c(str2num(seqs.lab$gateway[1]), name.suffix, ".pdf")), 
        width = width, height = height)
    print(gg + geom_rect(data=rects, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                         fill = "RED",
                         color="RED",
                         size = 1.1,
                         inherit.aes = FALSE)+ 
            theme_bw() + 
            theme(plot.title = element_text(family = font,
                                            face="bold", size=font.size)))
    dev.off()
  }
  setwd(oldwd)
  return(list(norm.df, rects))
}
####


#### Preproc for ARM
.remDur <- function(seq.datt){
  seq.datt$listo <- unlist(lapply(seq.datt$listo,
                                   function(x){strcat(num2str2(x, " "), " ")}))
  return(seq.datt)
}
.maskGoodBad <- function(seq.datt){
  ## mask the known good and error sequences
  mask.seq <- c("B C E F G",
                "C E F G",
                "C E F G H",
                "C B C E F G",
                "M O P H",
                "M O P H G",
                "R b c d e",
                "g i j e",
                "g i j e g",
                "M",
                "g",
                "R")
  for(ss in mask.seq)
  {
    seq.datt$listo[which(seq.datt$listo == ss)] <- "good"
  }
  seq.datt$listo[which(seq.datt$erc != 0)] <- "error"
  return(seq.datt)
}
preProcARM <- function(seq.datt){
  ## make gateway nicer
  gw <- str2num2(seq.datt$gateway)[3]
  seq.datt$gateway <- gw
  ## remove repeated notifications
  seq.datt <- remRepNot(seq.datt)
  ## remove durations
  seq.datt <- .remDur(seq.datt)
  seq.datt <- .maskGoodBad(seq.datt)
  return(seq.datt)
}

#### get norm.df
.getNormDf <- function(seqs.lab, f=7, agg.factor="day", tz="GMT"){
  library(xts)
  library(ggplot2)
  seq.lab.ts <- make.ts2(x = as.numeric(seqs.lab$listo),
                         f = f, pack = "xts", 
                         ord = as.POSIXct(seqs.lab$dat, tz=tz), tz=tz)
  seq.lab.agg <- aggTs(x = seq.lab.ts, br.in = agg.factor, func = "sum")
  seq.lab.agg <- make.ts2(x = seq.lab.agg$x, f = f, pack = "xts", 
                          ord = as.POSIXct(seq.lab.agg$Time, tz=tz), tz = tz)
  
#   seq.er.ts <- make.ts2(seqs.lab$erc, f, "xts", seqs.lab$dat, "GMT")
#   seq.er.agg <- aggTs(seq.er.ts, agg.factor, sum)
#   seq.er.agg <- make.ts2(seq.er.agg$x, f, "xts", 
#                          as.POSIXct(seq.er.agg$Time, tz), tz)
#   seq.er.agg[which(seq.er.agg != 0)] <- 1
  
  norm.df <- data.frame("Time" = index(seq.lab.agg), "x" = seq.lab.agg)
  row.names(norm.df) <- NULL
  return(norm.df)
}


#### generalized ARM (non sequential data)
getArulesGen <- function(vect, width, eleminate.item = NULL,
                         method.rule = c("roll", "mat", "roll2"),
                         minlen = 4, maxlen = 4,
                         minsup, minconf, ...){
  #### Execute apriori algorithm on given data
  library(zoo)
  library(arules)
  library(data.table)
  # frequent items - all items in data vector
  freq.pat <- unique(vect)
  if(!is.null(eleminate.item)){
    freq.pat <- freq.pat[!freq.pat %in% eleminate.item]
  }
  # method of converting to transactional data
  if(method.rule == "roll"){
    seq.mat <- data.frame(rollapply(vect, width, rbind))
  } 
  if(method.rule == "mat"){
    seq.mat <- data.frame(t(matrix(vect, nrow = width)))
  }
  if(method.rule == "roll2"){
    vect <- c(rep(0, width-1), vect, rep(0, width-1))
    seq.mat <- data.frame(rollapply(vect, width, rbind))
  }
  # Get transactions from data
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
                                    minlen = minlen, maxlen = maxlen))
  return(rules)
}



#############################################################################