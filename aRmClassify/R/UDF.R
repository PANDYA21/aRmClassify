#########################################################################
####                    User Defined Functions                      #####
#########################################################################

#### extract numbers from a string
str2num <- function(x)
{
  num <- as.character(gsub("[^[:digit:]]", "", as.character(x)))
  return(num)
}

#### extract characters from a string mixed with numbers
num2str <- function(x)
{
  string <- as.character(gsub("[[:digit:]]", "", as.character(x)))
  return(string)
}

# ###
# TO BE PRESERVED FOR FUTURE USE
# #### extract numbers' vector from a string
# #### use with lapply/sapply
# str2num2 <- function(x, Sep)
# {
#   if(missing(Sep)){
#     Sep = ""
#   }
#   num <- unlist(strsplit(x, strcat(c("[^[:digit:]]", Sep))))
#   num <- num[2:length(num)]
#   return(num)
# }
#
# #### extract character vector from a string mixed with numbers
# #### use with lapply/sapply
# num2str2 <- function(x, Sep)
# {
#   if(missing(Sep)){
#     Sep = ""
#   }
#   string <- as.character(gsub("[[:digit:]]", "", as.character(x)))
#   return(unlist(strsplit(string, Sep)))
# }
# ###

#### extract numbers' vector from a string
#### use with lapply/sapply
# str2num2 <- function(x, Sep)
# {
#   if(missing(Sep)){
#     Sep = ""
#     num <- unlist(strsplit(x, strcat(c("[^[:digit:]]", Sep))))
#     num <- num[2:length(num)]
#     return(num)
#   }else{
#     string <- as.character(gsub("[[:punct:]]", "", as.character(x)))
#     string <- as.character(gsub("[[:alpha:]] ", "", as.character(string)))
#     return(unlist(strsplit(string, Sep)))
#   }
# }
str2num2 <- function(x, Sep)
{
  if(missing(Sep)){
    Sep = ""
    num <- unlist(strsplit(x, strcat(c("[^[:digit:]]", Sep))))
    num <- num[2:length(num)]
    return(num)
  }else{
    string <- as.character(gsub("[[:punct:]]", "", as.character(x)))
    string <- as.character(gsub("[[:alpha:]] ", ".", as.character(string)))
    string <- as.character(gsub(" ", "", as.character(string)))
    string <- as.character(gsub("^\\.", "", as.character(string)))
    string <- as.character(gsub("\\.", " ", as.character(string)))
    return(unlist(strsplit(string, Sep)))
  }
}


#### extract character vector from a string mixed with numbers
#### use with lapply/sapply
# num2str2 <- function(x, Sep)
# {
#   if(missing(Sep)){
#     Sep = ""
#     string <- as.character(gsub("[[:digit:]]", "", as.character(x)))
#     return(unlist(strsplit(string, Sep)))
#   }else{
#     pat <- " [[:digit:]]+"
#     string <- as.character(gsub(pat, "", as.character(x)))
#     string <- as.character(gsub(" ", "  ", as.character(string)))
#     # string <- as.character(gsub(Sep, "", as.character(string)))
#     return(unlist(strsplit(string, strcat(c(Sep, " ")))))
#   }
# }
num2str2 <- function(x, Sep)
{
  if(missing(Sep)){
    Sep = ""
    string <- as.character(gsub("[[:digit:]]", "", as.character(x)))
    return(unlist(strsplit(string, Sep)))
  }else{
    pat <- "[[:digit:]]+"
    string <- as.character(gsub(pat, "", as.character(x)))
    string <- as.character(gsub("  ", " ", as.character(string)))
    string <- as.character(gsub(" ", "  ", as.character(string)))
    # string <- as.character(gsub(Sep, "", as.character(string)))
    return(unlist(strsplit(string, strcat(c(Sep, " ")))))
  }
}


#### extract alphabetical string from a string mixed with numbers or special chars
#### use with lapply/sapply
all2str <-function(x)
{
  ss <- as.character(gsub("[^[:alpha:]]", "", as.character(x)))
  return(ss)
}


#### make a time series of supported types
make.ts <- function(x, f)
{
  if (missing(f))
  {
    print("No freq defined! default will be considered.")
    f <- 1
  }
  if (class(x) == "numeric" | class(x) == "integer")
  {
    x.ts <- ts(x, frequency = f)
  }
  if (class(x) != "numeric" | class(x) != "integer")
  {
    x <- as.numeric(x)
    x.ts <- ts(x, frequency = f)
  }
  return(x.ts)
}


#### make a better time series from "ts" or "xts" packages
make.ts2 <- function(x, f, pack, ord, tz){
  if (missing(f))
  {
    print("No freq defined! default will be considered.")
    f <- 1
  }
  if (missing(pack))
  {
    pack = "xts"
  }

  if (pack == "ts")
  {
    if (class(x) == "numeric" | class(x) == "integer")
    {
      x.ts <- ts(x, frequency = f)
    }
    if (class(x) != "numeric" | class(x) != "integer")
    {
      x <- as.numeric(x)
      x.ts <- ts(x, frequency = f)
    }
  }

  if (missing(ord))
  {
    beg <- format(Sys.time(), tz="GMT")
    # endg <- format(Sys.time() + length(x)*3600*24, tz = "GMT")
    ord <- unlist(lapply(c(1:length(x)),
                         function(xx){
                           format(Sys.time() + xx*3600*24, tz = "GMT")
                         }))
    ord <- as.POSIXct(ord, tz = "GMT")
  }

  if (missing(tz))
  {
    tz <- "GMT"
  }

  if (pack == "xts")
  {
    x.ts <- xts(x = x, frequency = f, order.by = ord, tzone = tz)
  }

  return(x.ts)
}


#### get the source path in R, RStudio or Rscript
getScriptPath <- function(){
  cmd.args <- commandArgs()
  m <- regexpr("(?<=^--file=).+", cmd.args, perl=TRUE)
  script.dir <- dirname(regmatches(cmd.args, m))
  if(length(script.dir) == 0) # stop("can't determine script dir: please call the script with Rscript")
  {
    script.dir <- dirname(sys.frame(1)$ofile)
  }
  if(length(script.dir) > 1) stop("can't determine script dir: more than one '--file' argument detected")
  return(script.dir)
}


#### string concatanation
strcat <- function(x, Sep){
  if (missing(Sep))
  {
    Sep = ""
  }
  x <- paste(x, sep="", collapse=Sep)
  return(x)
}


#### string to time conversion
as.Time <- function(x) {
  x1 <- as.POSIXct(x, format="%H:%M:%S")
  x2 <- as.POSIXct(x, format="%H:%M")

  if (!is.na(x1)) x <- x1
  else if (!is.na(x2)) x <- x2

  return(x)
}


#### convert time to decimal hours
min2Dec <- function(x) {
  y <- sapply((strsplit(unlist(strsplit(x,"-")), ":")),
       function(x) {
         x <- as.numeric(x)
         x[1]+x[2]/60
       })

  y <- tail(y, 1)

  if (length(unlist(strsplit(x, "-"))) > 1)
  {
    if (y >0)
    {
      y <- -y
    }
  }

  return(y)
}


#### calculate remaining extra hours (Xtime)
calcHrs <- function(t.in, t.out, lunch.out, lunch.in, past) {
  t.in <- as.Time(t.in)
  t.out <- as.Time(t.out)

  if (t.in <= as.Time("9:00"))
  {
    breakfast <- min2Dec("0:15")
  }else
  {
    breakfast <- 0
  }

  if (!missing(lunch.in) & !missing(lunch.out))
  {
    lunch.in <- as.Time(lunch.in)
    lunch.out <- as.Time(lunch.out)
    x <- as.numeric(t.out - t.in) - 7 - breakfast - as.numeric(difftime(lunch.in,
                                                            lunch.out,
                                                            units = "hour"))
  }else
  {
    if (as.numeric(t.out - t.in) >= 3)
    {
      x <- as.numeric(t.out - t.in) - 7 - 0.5 - breakfast
    }else if (as.numeric(t.out - t.in) < 3)
    {
      x <- as.numeric(t.out - t.in) - 7 - breakfast
    }
  }

  if (!missing(past)) x <- x + min2Dec(past)
  else if (missing(past)) x <- x

  y <- x
  x <- format(round(x, 2), nsmall = 2)
  x <- as.numeric(unlist(strsplit(as.character(x),"\\.")))
  if (y > 0) x <- strcat(c(x[1], round(x[2]*0.6)) , ":")
  if (y < 0) x <- strcat(c(strcat(c("-", x[1])), round(x[2]*0.6)) , ":")

  return(x)
}


#### covert decimal to binary
dec2bin <- function(x)
{
  x <- paste(sapply(strsplit(paste(rev(intToBits(x))),""),`[[`,2),collapse="")
  return(x)
}


#### list functions from workspace
lsf <- function(){
  # lis <- ls()
  # lis <- .Internal(ls(name = .GlobalEnv, all.names = T))
  lis <- ls(name = .GlobalEnv, all.names = T)
  inde <- logical(0)
  for(i in 1:length(lis))
  {
    inde <- c(inde, is.function(eval(parse(text=lis[i]))))
  }
  return(lis[inde])
}


#### list objects with sizes from workspace
lsSize <- function(unit){
  if(missing(unit)){
    unit = "b"
  }
  lis <- ls(name = .GlobalEnv, all.names = T)
  size <- numeric(0)
  for(ll in lis)
  {
    si <- object.size(eval(parse(text=ll)))
    si <- format(si, units = unit)
    size <- c(size, si)
  }
  ret <- cbind(lis, size)
  return(ret[order(as.numeric(str2num(size)), decreasing = T),])
}


#### update UDF.R (only) file everywhere
updateUDF <- function(){
  file.copy(from = "D:/State_transition/Codes/Final/UDF.R",
            to = "D:/UDF.R",
            overwrite = T,
            copy.date = T)

#   file.copy(from = "D:/State_transition/Codes/Final/UDF.R",
#             to = "//bosch.com/dfsrb/DfsDE/LOC/We/TT/ESY2_Students/Pandya/UDF.R",
#             overwrite = T,
#             copy.date = T)
}


#### get tex output for writing colorul sequences (durations)
texSeq <- function(Seq){
  durs <- as.numeric(str2num2(Seq, " "))
  sts <- as.character(num2str2(Seq, " "))
  pst <- unlist(lapply(durs, function(xx){strcat(c("{\\color{blue} ", xx, "}"))}))
  return(cat(strcat(c(t(cbind(sts, pst))), " ")))
}



#### plot hist with percentage
histPerc <- function(x, ...){
  hh <- hist(x, plot = F)
  perc <- round((hh$counts / length(x)) * 100)
  plot(hh, yaxt = "n", ylab = "Percentage", ...)
  axis(side = 2, at = hh$counts, labels = perc)
}


#### Aggregate time series (flexible)
aggTs <- function(x, br.in, func){
  br.in <- as.character(br.in)
  agg <- aggregate(as.numeric(x), list(Time = cut(index(x), breaks=br.in)),
                   func, na.rm = TRUE)
  return(agg)
}



#### Plot pdfs using TikzDev library - tikz package from Tex
## start
plotStartTikz <- function(path = getwd(), nam = "tikz", width=10, height=6){
  library(tikzDevice)
  tf <- file.path(path, strcat(c(strcat(c(nam, ".tex")))))
  tikz(tf,standAlone=TRUE, width = width, height = height)
}
## plot here
## end
plotEndTikz <- function(path = getwd(), nam="tikz", compile.pdf=T){
  dev.off()
  if(compile.pdf == TRUE){
    ## generate .pdf plot
    tools::texi2dvi(strcat(c(nam, ".tex")),pdf=T,clean=T)
    # View the output
    system(paste(getOption('pdfviewer'),strcat(c(nam, ".pdf"))))
  }
}



#### get summary of which UDF is defined where
.getFunNames <- function(fl){
  sapply(fl,
         function(x){
           source(x, local = T)
           return(ls())
         })
}
getUDFsummary <- function(){
  setwd("D:/State_transition/Codes/Final/")
  fl <- list.files(pattern = "^U.*\\.R$")
  return(.getFunNames(fl))
}


#### shortcuts to edit UDF_xx.R files
editUDF <- function(file.name){
  file.edit(strcat(c("D:/State_transition/Codes/Final/", file.name)))
}


#### emulate default ggplot colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}




#### end
#########################################################################
