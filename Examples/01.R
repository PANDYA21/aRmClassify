### Load namespace functions
# source("R/UDF.R")
# source("R/UDF_ARM.R")
# source("R/UDF_SS.R")
# source("R/UDF_SS_part2.R")
# source("R/UDF_stats.R")
# source("R/UDF_synth.R")

### generate a random sequence with a pattern
set.seed(1234)
synthDat <- genRandSeq(slen = 100000, reg = 100, max.inc = 50, distr = "tangent")

### occurrence probabilities of each item and plot of the probabilities
ggDF <- data.frame(xx = synthDat, stringsAsFactors = TRUE)
ggDF$xx <- factor(ggDF$xx, levels = c(paste0("S", 1:30), "error"))
gg1 <- ggplot(data = ggDF, mapping = aes(x = xx)) +
  geom_bar(aes(y = (..count..)/sum(..count..)),
           fill = c(gg_color_hue(3), rep("grey50", 28))) +
  ylab("Occurrene probability") + xlab("Unique items") +
  theme_bw()
print(gg1)
# mean occurrence probability
meanAll <- mean(unlist(lapply(unique(synthDat), suppn, synthDat, 1)))
print(meanAll)
# mean of S1, S2, S3 i.e. affected items' occuerrence probability
meanFP <- mean(unlist(lapply(c("S1", "S2", "S3"), suppn, synthDat, 1)))
print(meanFP)
# percentage more
print(paste0(as.character(100*(meanFP - meanAll)/meanAll), "%"))

# # get Association rules with measure kulc
# widths <- seq(25,200,25)
# anss <- mclapply(
#   X = widths,
#   FUN = function(x){
#     getRules(
#       Vect = synthDat,
#       Width = x
#     )
#   },
#   mc.cores = 3
# )
#
# # boxplot(lapply(anss, function(x) x$df$kulcs[1:3]))
# # boxplot(lapply(anss, function(x) x$df$lift[1:3]))
#
# ansKulc <- unlist(lapply(anss, function(x) x$df$kulc[1:3]))
# gg2 <- ggplot(data = data.frame(xx = factor(rep(widths, each = 3)), yy = ansKulc)) +
#   geom_boxplot(aes(x = xx, y = yy))
#
# ansLift <- unlist(lapply(anss, function(x) x$df$lift[1:3]))
# gg3 <- ggplot(data = data.frame(xx = factor(rep(widths, each = 3)), yy = ansLift)) +
#   geom_boxplot(aes(x = xx, y = yy))
#
# print(gg2)
# print(gg3)


##
width <- 50
ans <- getRules(Vect = synthDat, Width = width)
ans$df[1:20,]

### classify
thres <- 1.5
FPdf <- ans$df[which(ans$df$kulcMenaPerc > thres), ]
FPitems <- unlist(getLhss(FPdf))
labDat <- as.numeric(synthDat %in% FPitems)
# labAgg <- matrix(data = labDat, ncol = 100, byrow = TRUE)
labAgg <- rollapply(labDat, 100, rbind)
labAgg <- apply(labAgg, 1, sum)
labAgg <- c(rep(0, length(labDat) - length(labAgg)), labAgg)

labAgg <- matrix(data = labAgg, ncol = 100, byrow = TRUE)
labAgg <- apply(labAgg, 1, sum)
labAgg <- minMaxNorm(labAgg)

labDat[which(synthDat == "error")] <- NA
labErr <- matrix(data = labDat, ncol = 100, byrow = TRUE)
labErr <- apply(labErr, 1, sum, na.rm = FALSE)

plot(labAgg, type = "l")
points(x = which(is.na(labErr)),
       y = rep(max(labAgg)*0.8,
               length(which(synthDat == "error"))),
       col = "RED")

# round(which(synthDat == "error"))
which(is.na(labErr))
which(labAgg > 0.565)

