#############################################################################
## User-Defined-Functions for state-transition / sequence analysis 
## Master Thesis: Bhaumik Pandya
#############################################################################


## covert burner start sequences (alphabetical) to vector of states (binary)
seq2state <- function(x, lev.all){
  if(missing(lev.all))
  {
    lev <- c("00000", "10000", "11000", "11100", "11110", "11111", "11101",
             "01101", "01100", "01000")
    bin.list <- unlist(lapply(c(0:31), dec2bin))
    bin.list <- unlist(lapply(bin.list, function(x) substr(x, nchar(x)-4, nchar(x))))
    levs <- bin.list # sort(unique(dt3$Burn.Start.State))
    lev.all <- c(lev, levs[!levs %in% lev])
  }
  
  labs <- c(LETTERS[1:length(lev.all)])
  labs2 <- c(letters[1:length(which(is.na(labs)))])
  labs[which(is.na(labs))] <- labs2
  rm(labs2)
  
  x <- as.character(x)
  x <- unlist(strsplit(x, ""))
  y <- character(0)
  for (i in 1:length(x))
  {
    y <- c(y, lev.all[which(labs == x[i])])
  }
  return(y)
}


## covert burner start sequences (alphabetical) to vector of states (binary) - 7bit
seq2state7bit <- function(x, lev.all){
  if(missing(lev.all))
  {
    lev <- c("0010000", 
             "1000000", 
             "1001000", 
             "1001100", 
             "1001110", 
             "1001111", 
             "1001101",
             "1011101",
             "0011101", 
             "0011001",
             "0011000",
             
             "1010000", 
             "1011000", 
             "1011100", 
             "1011110", 
             "1011111", 
             "1011101",
             "1011101",
             
             "0100000", 
             "0101000", 
             "0101100", 
             "0101110", 
             "0101111", 
             "0101101",
             "0111101",
             
             "0110000", 
             "0111000", 
             "0111100", 
             "0111110", 
             "0111111")
    
    lev.rem <- unlist(lapply(c(32:95), dec2bin))
    lev.rem <- unlist(lapply(lev.rem, 
                             function(x) substr(x, nchar(x)-6, nchar(x))))
    lev <- c(lev, lev.rem)
    
    bin.list <- unlist(lapply(c(0:127), dec2bin))
    bin.list <- unlist(lapply(bin.list, 
                              function(x) substr(x, nchar(x)-6, nchar(x))))
    levs <- bin.list
    lev.all <- c(lev, levs[!levs %in% lev])
    lev.all <- unique(lev.all)
  }
  
  labs <- c(LETTERS[1:18], letters[1:12],
            LETTERS[19:26], letters[13:26],
            paste0(LETTERS, "+"), paste0(letters, "+"),
            paste0(LETTERS[1:24], "-"))
  # labs[which(is.na(labs))] <- c(letters[1:length(which(is.na(labs)))])
  
  x <- as.character(x)
  x <- unlist(strsplit(x, " "))
  y <- character(0)
  for (i in 1:length(x))
  {
    y <- c(y, lev.all[which(labs == x[i])])
  }
  return(y)
}



## View seq2state output as split elements and in data.frame View with durations
ViewSeq2state <- function(x, dur, state){
  if(missing(dur)){
    dur = FALSE
  }
  if(missing(state)){
    state = TRUE
  }
  if(dur == FALSE){
    xdf <- data.frame(t(data.frame(lapply(seq2state(x), 
                                          function(xx) strsplit(xx, "")))))
    xdf <- data.frame(cbind(num2str2(x), xdf))
    row.names(xdf) <- NULL
    names(xdf) <- c("STATE","REQ","FAN","GAS","IGN","FLM")
    return(xdf)
    # View(xdf)
  }else{
    xdf <- data.frame(t(data.frame(lapply(seq2state(x), 
                                          function(xx) strsplit(xx, "")))))
    xdf <- data.frame(cbind(num2str2(x), xdf))
    if(is.na(as.numeric(str2num(x)))){
      durs <- rep(NA, length(xdf[,1]))
    }else{
      durs <- str2num2(x)
      if(length(durs) < length(xdf[,1])){
        durs <- c(durs, rep(NA, (length(xdf[,1]) - length(durs))))
      }
    }
    xdf <- data.frame(cbind(xdf, durs))
    row.names(xdf) <- NULL
    names(xdf) <- c("STATE","REQ","FAN","GAS","IGN","FLM","DUR")
    return(xdf)
    # View(xdf)
  }
}



## View seq2state output for 7 bit states as split elements and in data.frame View
## with durations
ViewSeq2state7bit <- function(x, dur, state, ...){
  if(missing(dur)){
    dur = FALSE
  }
  if(missing(state)){
    state = TRUE
  }
  if(dur == FALSE){
    xdf <- data.frame(t(data.frame(lapply(seq2state7bit(x, ...), 
                                          function(xx) strsplit(xx, "")))))
    xdf <- data.frame(cbind(num2str2(x, " "), xdf))
    row.names(xdf) <- NULL
    names(xdf) <- c("STATE","CH","DHW","SP","FAN","GAS","IGN","FLM")
    return(xdf)
    # View(xdf)
  }else{
    xdf <- data.frame(t(data.frame(lapply(seq2state7bit(x, ...), 
                                          function(xx) strsplit(xx, "")))))
    xdf <- data.frame(cbind(num2str2(x, " "), xdf))
    if(is.na(as.numeric(str2num(x)))){
      durs <- rep(NA, length(xdf[,1]))
    }else{
      durs <- str2num2(x, " ")
      if(length(durs) < length(xdf[,1])){
        durs <- c(durs, rep(NA, (length(xdf[,1]) - length(durs))))
      }
    }
    xdf <- data.frame(cbind(xdf, durs))
    row.names(xdf) <- NULL
    names(xdf) <- c("STATE","CH","DHW","SP","FAN","GAS","IGN","FLM","DUR")
    return(xdf)
    # View(xdf)
  }
}



## combine burner start states and duration of each state in secs into 
## one string sequence i.e. burner start sequence string
getSeq <- function(x){
  
  seq <- character(0)
  dur <- numeric(0)
  durs <- numeric(0)
  seq.dur <- character(0)
  er <- numeric(0)
  erc <- numeric(0)
  dt3 <- x
  
  for (i in 2:dim(dt3)[1])
  {
    if (dt3$Request[i]==1 & dt3$Request[i-1]==0)
    {
      seq.dur <- c(seq.dur, strcat(paste0(seq, durs)))
      seq <- character(0)
      seq <- as.character(dt3$label[i])
      durs <- numeric(0)
      erc <- c(erc, sum(er))
      er <- numeric(0)
    } else if (dt3$Request[i]==1 & dt3$Request[i-1]==1)
    {
      if (as.character(dt3$label[i]) != as.character(dt3$label[i-1]))
      {
        seq <- c(seq, as.character(dt3$label[i]))
        durs <- c(durs, sum(dur))
        dur <- numeric(0)
      }else
      {
        nu <- as.numeric(difftime(dt3$DateTime[i], 
                                  dt3$DateTime[i-1], 
                                  unit="secs"))
        # nu <- round((nu)/10)*10 # round the seconds to a group
        dur <- c(dur, nu)
        er <- c(er, dt3$"Operating_status:_Error_Locking"[i])
      }
    }
  }
  
  return(data.table(seq.dur, erc))
}


## combine burner start states and duration of each state in secs into 
## one string sequence i.e. burner start sequence string
getSeq2 <- function(x)
{
  dt3 <- x
  inde <- which(dt3$Request == 1)
  lab <- dt3$label
  
  if(length(inde) == 0){
    return(integer(0))
  }else {
    Breaks <- c(0, which(diff(inde) != 1), length(inde)) 
    lis <- sapply(seq(length(Breaks) - 1),
                  function(i) inde[(Breaks[i] + 1):Breaks[i+1]]) 
    
    listoo <- sapply(lis, function(x) lab[x])
    dur <- lapply(lis, function(x) diff(dt3$DateTime[x[1]:x[length(x)]],
                                        units.difftime="secs"))
    
    listo <- lapply(c(1:length(listoo)), 
                    function(x) paste(listoo[[x]], dur[[x]],
                                      collapse = "", sep = ""))
    
    erc <- sapply(lis, function(x) sum(dt3$"Operating_status:_Error_Locking"[x[1]:x[length(x)]]))
    dat <- unlist(lapply(lis, function(x) as.character(dt3$DateTime[x[1]])))
    dat <- as.POSIXct(dat, tz = "GMT")
    return(data.table(dat, listo, erc))
  }
}


### Refine getSeq2 (the faster) function
### remove repeated states and add the duration of them
refineGetSeq2 <- function(listo){
  ## extract indexes of non-repeatative elements
  fac <- lapply(num2str(listo), 
               function(x) factor(unlist(strsplit(as.character(x), ""))))
  dif <- lapply(fac, function(x) diff(as.numeric(x)))
  inde <- lapply(dif, function(x) which(x!=0))
  
  se <- lapply(c(1:length(inde)), 
               function(j) strcat( c(as.character(unlist(fac[j])[unlist(inde[j])]), 
                                     as.character(fac[[j]][length(fac[[j]])])) ))
  
  ## get indexes to sum the durations
  inde <- lapply(inde, 
                 function(x) c(1, x, 
                               length(unlist(fac[which(lapply(inde, identical, x) == TRUE)]))))
  
  ## sum the durations of same consecutive states
  summ <- lapply( c(1:length(inde)), 
                  function(i) 
                  {
                    unlist(
                      lapply(c(1:(length(inde[[i]])-1)), 
                              function(j) 
                              { 
                                sum(as.numeric(str2num2(listo[i])[inde[[i]][j]:inde[[i]][j+1]]),
                                               na.rm=T) 
                              }
                          )
                      )
                  })
  
  ## merge states and durations
  se.sep <- lapply(se, function(x) unlist(strsplit(x, "")))
  
  ##
  seq.dur.fast <- unlist(lapply( c(1:length(summ)), 
                                  function(i) paste(se.sep[[i]], summ[[i]], 
                                                    collapse = "", sep = "")))
  
  return(seq.dur.fast)
}



##### next version
##### for data with CH and DHW separation and included Temp_set_point flag
getSeq3 <- function(x)
{
  dt3 <- x
  inde <- which(dt3$Request == 1)
  lab <- dt3$label
  
  if(length(inde) == 0){
    return(integer(0))
  }else {
    Breaks <- c(0, which(diff(inde) != 1), length(inde)) 
    lis <- sapply(seq(length(Breaks) - 1),
                  function(i) inde[(Breaks[i] + 1):Breaks[i+1]]) 
    
    listoo <- sapply(lis, function(x) lab[x])
    dur <- lapply(lis, function(x) diff(dt3$DateTime[x[1]:x[length(x)]],
                                        units.difftime="secs"))
    
    listo <- lapply(c(1:length(listoo)), 
                    function(x) paste(listoo[[x]], dur[[x]],
                                      collapse = " ", sep = " "))
    
    erc <- sapply(lis, function(x) sum(dt3$"Operating_status:_Error_Locking"[x[1]:x[length(x)]]))
    dat <- unlist(lapply(lis, function(x) as.character(dt3$DateTime[x[1]])))
    dat <- as.POSIXct(dat, tz = "GMT")
    return(data.table(dat, listo, erc))
  }
}


##### parallel version
##### for data with CH and DHW separation and included Temp_set_point flag
getSeq3par <- function(x)
{
  dt3 <- x
  inde <- which(dt3$Request == 1)
  lab <- dt3$label
  
  if(length(inde) == 0){
    return(integer(0))
  }else {
    Breaks <- c(0, which(diff(inde) != 1), length(inde)) 
    lis <- sapply(seq(length(Breaks) - 1),
                  function(i) inde[(Breaks[i] + 1):Breaks[i+1]])
    
    listoo <- sapply(lis, function(x) lab[x])
    dur <- mclapplyWin(lis, function(x) diff(dt3$DateTime[x[1]:x[length(x)]],
                                        units.difftime="secs"))
    
    listo <- mclapplyWin(c(1:length(listoo)), 
                    function(x) paste(listoo[[x]], dur[[x]],
                                      collapse = " ", sep = " "))
    
    erc <- unlist(mclapplyWin(lis, 
                              function(x){
                                sum(dt3$"Operating_status:_Error_Locking"[x[1]:x[length(x)]])
                              }))
    
    dat <- unlist(lapply(lis, function(x) as.character(dt3$DateTime[x[1]])))
    dat <- as.POSIXct(dat, tz = "GMT")
    return(data.table(dat, listo, erc))
  }
}


##### next version
##### for data with CH and DHW separation and included Temp_set_point flag
refineGetSeq3 <- function(listo){
  ## extract indexes of non-repeatative elements
  fac <- lapply(num2str(listo), 
                function(x) factor(unlist(strsplit(as.character(x), "  "))))
  dif <- lapply(fac, function(x) diff(as.numeric(x)))
  inde <- lapply(dif, function(x) which(x!=0))
  
  se <- lapply(c(1:length(inde)), 
               function(j) strcat( c(as.character(unlist(fac[j])[unlist(inde[j])]), 
                                     as.character(fac[[j]][length(fac[[j]])])) , Sep = " "))
  
  ## get indexes to sum the durations
  inde <- lapply(inde, 
                 function(x) c(1, x, 
                               length(unlist(fac[which(lapply(inde, identical, x) == TRUE)]))))
  
  ## sum the durations of same consecutive states
  summ <- lapply( c(1:length(inde)), 
                  function(i) 
                  {
                    unlist(
                      lapply(c(1:(length(inde[[i]])-1)), 
                             function(j) 
                             { 
                               sum(as.numeric(str2num2(listo[i])[inde[[i]][j]:inde[[i]][j+1]]),
                                   na.rm=T) 
                             }
                      )
                    )
                  })
  
  ## merge states and durations
  se.sep <- lapply(se, function(x) unlist(strsplit(x, " ")))
  
  ##
  seq.dur.fast <- unlist(lapply( c(1:length(summ)), 
                                 function(i) paste(se.sep[[i]], summ[[i]], 
                                                   collapse = " ", sep = " ")))
  
  return(seq.dur.fast)
}



#### The Latest Version
#### for data with CH and DHW separation and included Temp_set_point flag
refineGetSeq4 <- function(listo){
  labs <- c(LETTERS, letters,
            unlist(lapply(LETTERS, function(x){strcat(c(x, "+"))})),
            unlist(lapply(letters, function(x){strcat(c(x, "+"))})),
            unlist(lapply(LETTERS[1:24], function(x){strcat(c(x, "-"))})))
  liston <- unlist(lapply(listo, 
                          function(xx){
                             for(ll in labs)
                             {
                               xx <- remTogg(xx, ll ,ll)
                             }
                             return(xx)
                           }))
  return(liston)
}
####


#### The Latest Version - parallelized
#### for data with CH and DHW separation and included Temp_set_point flag
refineGetSeq5 <- function(listo){
  labs <- c(LETTERS, letters,
            unlist(lapply(LETTERS, function(x){strcat(c(x, "+"))})),
            unlist(lapply(letters, function(x){strcat(c(x, "+"))})),
            unlist(lapply(LETTERS[1:24], function(x){strcat(c(x, "-"))})))
  liston <- unlist(mclapplyWin(listo, 
                               function(xx){
                                 for(ll in labs)
                                 {
                                   xx <- remTogg(xx, ll ,ll)
                                 }
                                 return(xx)
                               }))
  return(liston)
}


####
extractStateDur <- function(Seq, State){
  require(stringr)
  if(length(grep("[[:punct:]]", State)) == 0){
    pat <- strcat(c(State, " [[:digit:]]*"))
  }else{
    st <- unlist(strsplit(State, ""))
    pat <- strcat(c(st[1], "\\", st[2], " [[:digit:]]*"))
  }
  
  xxx <- unlist(str_extract_all(Seq, pat))
  xxx <- str2num2(xxx, " ")
  return(as.numeric(xxx))
}

####
extractDdur <- function(x){
  xxx <- x
  strsp <- unlist(strsplit(gsub("D [[:digit:]]*", " ", xxx), " "))
  for(i in 1:length(strsp))
  {
    xxx <- gsub(strsp[i], "", xxx)
  }
  return(as.numeric(str2num2(xxx)))
}

####
extractGdur <- function(x){
  xxx <- x
  strsp <- unlist(strsplit(gsub("*G[0-9]*", " ", xxx), " "))
  for(i in 1:length(strsp))
  {
    xxx <- gsub(strsp[i], "", xxx)
  }
  return(as.numeric(str2num2(xxx)))
}


#### extract ON state duration while CH request
extractCHONdur <- function(Seq, ...){
  return(as.numeric(as.character(str2num2(Seq, " ")[grep("^(1).*101$" ,seq2state7bit(Seq, ...))])))
}


#### extract ON state duration while DHW request
extractDHWONdur <- function(Seq, ...){
  return(as.numeric(as.character(str2num2(Seq, " ")[grep("^(01).*101$" ,seq2state7bit(Seq, ...))])))
}



### Filter out flame sensor test toggling in 7bit states
fanFilt <- function(Seq, rep.list, ...){
  if(missing(rep.list)){
    all.states <- ViewSeq2state7bit(labs)
    all.states[,c(2:8)] <- apply(all.states[,c(2:8)], 2, as.numeric)
    
    rep.list <- character(0)
    for(ii in 1:128)
    {
      if(all.states$FAN[ii] == 0){
        if(length(rep.list) != 0){
          if(all.states[ii,1] %in% rep.list[,2]){
            next
          }
        }
        for(jj in 1:128)
        {
          if(ii == jj){
            next
          }else{
            if(all.states[ii, 2] == all.states[jj, 2]){
              if(all.states[ii, 3] == all.states[jj, 3]){
                if(all.states[ii, 4] == all.states[jj, 4]){
                  if(all.states[ii, 6] == all.states[jj, 6]){
                    if(all.states[ii, 7] == all.states[jj, 7]){
                      if(all.states[ii, 8] == all.states[jj, 8]){
                        rep.list <- rbind(rep.list, 
                                          c(as.character(all.states[ii, 1]), 
                                            as.character(all.states[jj, 1])))
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  for(ii in 1:dim(rep.list)[1])
  {
    pat <- unlist(strsplit(rep.list[ii,1], ""))
    if(length(pat) == 2){
      patt <- strcat(c(pat[1], "\\", pat[2], " "))
    }else{
      patt <- strcat(c(pat, " "))
    }
    Seq <- gsub(pattern = patt,
                replacement = strcat(c(rep.list[ii,2], " ")) ,
                x = Seq,
                ...)
  }
  return(Seq)
}


#### calculate  CDF for a numerical-sequence
##   for plotSeq
cdf <- function(x){
  len <- length(x)
  cum <- numeric(0)
  for(i in 1:len)
  {
    cum <- c(cum, sum(x[1:i], na.rm = T))
  }
  return(cum)
}

#### insert a row into existing df
##   for plotSeq
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  return(existingDF)
}

#### plot the sequence
plotSeq <- function(Seq, Time, main, ylim, ...){
  td <- as.numeric(as.character(ViewSeq2state7bit(Seq, T)$DUR))
  tt <- cdf(td)
  # tt <- Sys.time() + cdf(tt)
  if(missing(Time)){
    Time <- Sys.time()
    tt <- Time + c(0, tt[1:length(tt)-1])
  }else{
    tt <- Time + c(0, tt[1:length(tt)-1])
  }
  
  
  dat <- data.frame(tt, ViewSeq2state7bit(Seq, T))
  dat[,c(3:10)] <- apply(dat[,c(3:10)], 2, function(xx){ as.numeric(as.character(xx)) })
  dat$STATE <- as.character(dat$STATE)
  datn <- dat
  cou <- 0
  for(ii in 1:(length(dat[,1]) - 1))
  {
    if(td[ii] > 1){
      for(jj in 2:td[ii])
      {
        cou <- cou + 1
        newrow <- dat[ii,]
        newrow$tt <- newrow$tt + jj - 1
        newrow$DUR <- 0
        datn <- insertRow(existingDF = datn, newrow = newrow, r = ii+cou)
      }
    }
  }
  
  
  for(ii in 1:as.numeric(as.character(datn$DUR[length(datn$DUR)])))
  {
    newrow <- datn[length(datn$DUR),]
    newrow$tt <- newrow$tt + 1
    newrow$DUR <- 0
    datn <- rbind.data.frame(datn, newrow)
  }
  rownames(datn) <- NULL
  
  
  tt <- c(0, cdf(diff(datn$tt)))
  
  if(missing(main)){
         main="Burner Start-up Cycle" 
  }
  
  if(missing(ylim)){
    ylim = c(0,2.5)
  }
  
  
  plot(y = as.numeric(as.character(datn$FLM))*2, 
       x = tt, yaxt = "n", xaxt = "n", ylim = ylim,
       type="l", col="RED", lwd=2.5, main=main, 
       xlab="Duration (sec)", ylab="ON/OFF", ...)
  
  axis(1, c(0:max(tt)))
  
  lines(y = as.numeric(as.character(datn$FAN))*1.2,
        x = tt, col="GREY", lwd=2)
  
  lines(y = as.numeric(as.character(datn$GAS))*1.3,
        x = tt, col="ORANGE", lwd=1)
  
  lines(y = as.numeric(as.character(datn$DHW))*1.6,
        x = tt, col="BROWN", lwd=2)
  
  lines(y = as.numeric(as.character(datn$CH))*1.8,
        x = tt, col="BLACK", lwd=2)
  
  lines(y = as.numeric(as.character(datn$SP))*2, 
        x = tt, col="BLUE", lwd=1.5)
  
  lines(y = as.numeric(as.character(datn$IGN))*1.4,
        x = tt, col="GREEN", lwd=2)
  
  legend("topright", legend = c("DHW", "CH", "FAN", "GAS", "IGN", "FLM", "SP"), 
         col = c("BROWN", "BLACK", "GREY", "ORANGE", "GREEN", "RED", "BLUE"), lwd=2.0)
}


##### plotSeq for 5 bit old states
plotSeq5bit <- function(Seq, Time, main, ...){
  td <- as.numeric(as.character(ViewSeq2state(Seq, T)$DUR))
  tt <- cdf(td)
  # tt <- Sys.time() + cdf(tt)
  if(missing(Time)){
    Time <- Sys.time()
    tt <- Time + c(0, tt[1:length(tt)-1])
  }else{
    tt <- Time + c(0, tt[1:length(tt)-1])
  }
  
  
  dat <- data.frame(tt, ViewSeq2state(Seq, T))
  dat[,c(2:6)] <- apply(dat[,c(2:6)], 2, function(xx){ as.numeric(as.character(xx)) })
  dat$STATE <- as.character(dat$STATE)
  datn <- dat
  cou <- 0
  for(ii in 1:(length(dat[,1]) - 1))
  {
    if(td[ii] > 1){
      for(jj in 2:td[ii])
      {
        cou <- cou + 1
        newrow <- dat[ii,]
        newrow$tt <- newrow$tt + jj - 1
        newrow$DUR <- 0
        datn <- insertRow(existingDF = datn, newrow = newrow, r = ii+cou)
      }
    }
  }
  
  
  for(ii in 1:as.numeric(as.character(datn$DUR[length(datn$DUR)])))
  {
    newrow <- datn[length(datn$DUR),]
    newrow$tt <- newrow$tt + 1
    newrow$DUR <- 0
    datn <- rbind.data.frame(datn, newrow)
  }
  rownames(datn) <- NULL
  
  
  tt <- c(0, cdf(diff(datn$tt)))
  
  if(missing(main)){
    plot(y = as.numeric(as.character(datn$FLM))*2, 
         x = tt, yaxt = "n", xaxt = "n", #ylim = c(0,2)
         type="l", col="RED", lwd=2.5, main="Burner Start-up Cycle", 
         xlab="Duration (sec)", ylab="ON/OFF", ...)
  }else{
    plot(y = as.numeric(as.character(datn$FLM))*2, 
         x = tt, yaxt = "n", xaxt = "n", #ylim = c(0,2)
         type="l", col="RED", lwd=2.5, main = main,
         xlab="Duration (sec)", ylab="ON/OFF", ...)
  }
  
  axis(1, c(0:max(tt)))
  
  lines(y = as.numeric(as.character(datn$FAN))*1.2,
        x = tt, col="GREY", lwd=2)
  
  lines(y = as.numeric(as.character(datn$GAS))*1.3,
        x = tt, col="ORANGE", lwd=1)
  
  lines(y = as.numeric(as.character(datn$IGN))*1.4,
        x = tt, col="GREEN", lwd=2)
  
  lines(y = as.numeric(as.character(datn$REQ))*1.6,
        x = tt, col="BLACK", lwd=2)
  
  
  legend("topright", legend = c("REQ", "FAN", "GAS", "IGN", "FLM"), 
         col = c("BLACK","GREY", "ORANGE", "GREEN", "RED"), lwd=2.0)
}



#### adjust the spaces in a sequence between states and duarations
spaceAdjSeq <- function(Seq){
  states <- num2str2(Seq, " ")
  durs <- str2num2(Seq, " ")
  Seq <- strcat(as.vector(t(cbind(states, durs))), Sep = " ")
  return(Seq)
}



#### translate 7 bit sequences into 5 bit sequences
translateSeqTo5bit <- function(Seq7, lev.all5, Sep, ...){
  xdf <- ViewSeq2state7bit(Seq7, T)
  xdf[,c(2:9)] <- sapply(xdf[,c(2:9)], as.character)
  xdf[,c(2:9)] <- sapply(xdf[,c(2:9)], as.numeric)
  req <- as.numeric( xdf$CH | xdf$DHW )
  xdf.n <- data.frame("STATE" = xdf$STATE, "REQ" = req, xdf[,c(5:8)])
  # seq2state("A")
  
  if(missing(lev.all5)){
    lev5 <- c("00000", "10000", "11000", "11100", "11110", "11111", "11101",
              "01101", "01100", "01000")
    bin.list5 <- unlist(lapply(c(0:31), dec2bin))
    bin.list5 <- unlist(lapply(bin.list5, function(x) substr(x, nchar(x)-4, nchar(x))))
    levs5 <- bin.list5 # sort(unique(dt3$Burn.Start.State))
    lev.all5 <- c(lev5, levs5[!levs5 %in% lev5])
  }
  labs5 <- c(LETTERS[1:length(lev.all5)])
  labs2 <- c(letters[1:length(which(is.na(labs5)))])
  labs5[which(is.na(labs5))] <- labs2
  rm(labs2)
  
  sts <- apply(xdf.n[,c(2:6)], 1, function(x){strcat(as.character(x))})
  newsts <- character(0)
  for(ii in 1:length(sts))
  {
    newsts <- c(newsts, labs5[which(lev.all5 == sts[ii])])
  }
  
  new.seq <- as.vector(t(cbind(newsts, xdf$DUR)))
  if(missing(Sep)){
    Sep = ""
  }
  new.seq <- strcat(new.seq, Sep)
  return(new.seq)
}


#### identify bad starts
identifyBadStart <- function(Seq){ 
  xdf <- ViewSeq2state7bit(Seq, T) 
  xdf[,c(2:9)] <- sapply(xdf[,c(2:9)], as.character) 
  xdf[,c(2:9)] <- sapply(xdf[,c(2:9)], as.numeric) 
  xdf[,1] <- as.character(xdf[,1]) 
  count <- 0 
  if(sum(xdf$CH) == length(xdf$CH)){ 
    flmdif <- c(0, diff(xdf$FLM)) 
    if(any(flmdif == -1)){ 
      for(ww in which(flmdif == -1))
      { 
        if(xdf$DUR[ww - 1] <= 10){ 
          count <- count + 1 # length(which(flmdif == -1)) 
        } 
      } 
    } 
    return(count) 
  }else if(all(xdf$DHW) == 1){ 
    flmdif <- c(0, diff(xdf$FLM))
    wh <- which(flmdif == -1) 
    if(any(xdf$SP[wh]) == 0){ 
      for(ww in which(flmdif == -1)) 
      { 
        if(xdf$DUR[ww - 1] <= 3){ 
          count <- count + 1 # length(which(xdf$SP[wh] == 0)) 
        }
      }
    } 
    return(count) 
  }else{ 
    Seq.sep <- separateCHDHWSeq(Seq) 
    for(ii in 1:length(Seq.sep)) 
    { 
      count <- count + identifyBadStart(Seq.sep[ii]) 
    } 
    return(count) 
  } 
} 


#### identify failed starts
identifyFailedStart <- function(Seq){ 
  xdf <- ViewSeq2state7bit(Seq, T) 
  xdf[,c(2:9)] <- sapply(xdf[,c(2:9)], as.character) 
  xdf[,c(2:9)] <- sapply(xdf[,c(2:9)], as.numeric) 
  xdf[,1] <- as.character(xdf[,1]) 
  count <- 0 
  if(sum(xdf$CH) == length(xdf$CH)){ 
    igndif <- c(0, diff(xdf$IGN)) 
    if(length(which(igndif == -1)) > 1){
      count <- count + length(which(igndif == -1)) 
    } 
    return(count) 
  }else if(all(xdf$DHW) == 1){ 
    igndif <- c(0, diff(xdf$IGN))
    wh <- which(igndif == -1) 
    if(length(which(igndif == -1)) > 1){ 
      if(any(xdf$SP[wh]) == 0){ 
        count <- count + length(which(xdf$SP[wh] == 0)) 
      } 
    } 
    return(count) 
  }else{ 
    Seq.sep <- separateCHDHWSeq(Seq) 
    for(ii in 1:length(Seq.sep)) 
    { 
      count <- count + identifyFailedStart(Seq.sep[ii]) 
    } 
    return(count) 
  } 
} 

#### separate CH adn DHW cycles
separateCHDHWSeq <- function(Seq){ 
  xdf <- ViewSeq2state7bit(Seq, T) 
  xdf[,c(2:9)] <- sapply(xdf[,c(2:9)], as.character) 
  xdf[,c(2:9)] <- sapply(xdf[,c(2:9)], as.numeric) 
  xdf[,1] <- as.character(xdf[,1]) 
  whch <- which(xdf$CH == 1) 
  whdhw <- which(xdf$DHW == 1) 
  chdif <- diff(whch) 
  dhwdif <- diff(whdhw) 
  loc.ch <- c(0, which(chdif > 1), length(whch)) 
  loc.dhw <- c(0, which(dhwdif > 1), length(whdhw)) 
  Seq.ch <- character(0) 
  for(ii in 1:(length(loc.ch) - 1)) 
  { 
    this <- strcat(rbind(xdf$STATE[ whch[(loc.ch[ii]+1):loc.ch[ii+1]] ], 
                         xdf$DUR[ whch[(loc.ch[ii]+1):loc.ch[ii+1]] ]), " ") 
    Seq.ch <- c(Seq.ch, this) 
  } 
  Seq.dhw <- character(0) 
  for(ii in 1:(length(loc.dhw) - 1)) 
  { 
    this <- strcat(rbind(xdf$STATE[ whdhw[(loc.dhw[ii]+1):loc.dhw[ii+1]] ], 
                         xdf$DUR[ whdhw[(loc.dhw[ii]+1):loc.dhw[ii+1]] ]), " ") 
    Seq.dhw <- c(Seq.dhw, this) 
  } 
  return(as.list(c("Seq.ch" = Seq.ch, "Seq.dhw" = Seq.dhw))) 
} 


#### Extract transitions from sequence
getTransitions <- function(seq.train){
  loc.er <- which(seq.train$erc != 0)
  seq.train$listo[loc.er] <- "error"
  trans.train <- lapply(seq.train$listo, 
                        function(x){
                          return(rollapply(unlist(strsplit(x, " ")) 
                                           , 2, rbind))
                        })
  trans.train <- lapply(c(1:length(trans.train)),
                        function(ii){
                          if(length(trans.train[[ii]]) != 0){
                            return(apply(trans.train[[ii]], 
                                         1, strcat, " "))
                          } else {
                            return(seq.train$listo[ii])
                          }
                        })
  trans.train <- lapply(c(1:length(trans.train)),
                        function(ii){
                          ans <- data.frame(matrix(nrow = length(trans.train[[ii]]),
                                                   ncol = 4))
                          names(ans) <- names(seq.train)
                          ans$gateway <- seq.train$gateway[ii]
                          ans$dat <- seq.train$dat[ii]
                          ans$listo <- trans.train[[ii]]
                          ans$erc <- seq.train$erc[ii]
                          return(data.table(ans))
                        })
  trans.train <- rbindlist(trans.train)
  return(trans.train)
}




#############################################################################