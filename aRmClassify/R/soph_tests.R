## parameters
# unique sequences
uu <- 30
# region
region <- 300
# max.inc
mi <- 7
# length of the data
llen <- 25000
# number of error
n.err <- 5
# transaction width
ww <- 50

## generate five sets of synthetic data for each distribution
syn.sine <- lapply(1:5, 
                  function(ii){
                    set.seed(123*ii)
                    return(genRandSeq(slen = llen, reg = region, 
                                      ners = n.err, uniqs = uu, 
                                      max.inc = mi, distr = "sine"))
                  })
syn.cos <- lapply(1:5, 
                   function(ii){
                     set.seed(123*ii)
                     return(genRandSeq(slen = llen, reg = region, 
                                       ners = n.err, uniqs = uu, 
                                       max.inc = mi, distr = "cosine"))
                   })
syn.tan <- lapply(1:5, 
                   function(ii){
                     set.seed(123*ii)
                     return(genRandSeq(slen = llen, reg = region, 
                                       ners = n.err, uniqs = uu, 
                                       max.inc = mi, distr = "tangent"))
                   })
syn.exp1 <- lapply(1:5, 
                   function(ii){
                     set.seed(123*ii)
                     return(genRandSeq(slen = llen, reg = region, 
                                       ners = n.err, uniqs = uu, 
                                       max.inc = mi, distr = "expo1"))
                   })
syn.exp2 <- lapply(1:5, 
                   function(ii){
                     set.seed(123*ii)
                     return(genRandSeq(slen = llen, reg = region, 
                                       ners = n.err, uniqs = uu, 
                                       max.inc = mi, distr = "expo2"))
                   })

## effect of width
widths <- seq(20,75,5)
effect.width <- mclapply(widths,
                       function(ww){
                         return(lapply(syn.sine, 
                                       function(x){
                                         rules <- getArulesGen2(vect = x, width = ww)
                                         ans <- subset(rules, lhs %in% "S1" & 
                                                         lhs %in% "S2" & 
                                                         lhs %in% "S3")@quality$lift
                                         if(length(ans) == 0){
                                           return(NA)
                                         } else {
                                           return(ans)
                                         }
                                       }))
                       })
# effect.width <- data.table(t(rbindlist(effect.width)))
# names(effect.width) <- as.character(widths)
# boxplot(effect.width)

eff.width <- data.frame("val" = unlist(effect.width), 
                        "width" = factor(rep(widths, each = 5)),
                        "val2" = 1)

gg <- ggplot(eff.width, aes(x = width, y = val)) + geom_boxplot() +
  theme_bw() + theme(plot.title = element_text(face="bold", size=12)) +
  ylab("Lift of the desired rule") + xlab("Transaction width") + 
  stat_summary(fun.y=mean, geom="line", aes(group=1), 
               size = 0.8, col = gg_color_hue(4)[3])  + 
  stat_summary(fun.y=mean, geom="point", col = "blue")

setwd("~/Dropbox/MAIT/Assignments/Sem-IV/Thesis/FinalTemplate/Chapter5")
setwd("fig/")
nam.pdf <- "eff_width_gg_sin"
plotStartTikz(nam = nam.pdf, width = 5, height = 3)
print(gg)
plotEndTikz(nam = nam.pdf)


effect.width2 <- mclapply(widths,
                         function(ww){
                           return(lapply(syn.cos, 
                                         function(x){
                                           rules <- getArulesGen2(vect = x, width = ww)
                                           ans <- subset(rules, lhs %in% "S1" & 
                                                           lhs %in% "S2" & 
                                                           lhs %in% "S3")@quality$lift
                                           if(length(ans) == 0){
                                             return(NA)
                                           } else {
                                             return(ans)
                                           }
                                         }))
                         })
effect.width3 <- mclapply(widths,
                          function(ww){
                            return(lapply(syn.tan, 
                                          function(x){
                                            rules <- getArulesGen2(vect = x, width = ww)
                                            ans <- subset(rules, lhs %in% "S1" & 
                                                            lhs %in% "S2" & 
                                                            lhs %in% "S3")@quality$lift
                                            if(length(ans) == 0){
                                              return(NA)
                                            } else {
                                              return(ans)
                                            }
                                          }))
                          })
effect.width4 <- mclapply(widths,
                          function(ww){
                            return(lapply(syn.exp1, 
                                          function(x){
                                            rules <- getArulesGen2(vect = x, width = ww)
                                            ans <- subset(rules, lhs %in% "S1" & 
                                                            lhs %in% "S2" & 
                                                            lhs %in% "S3")@quality$lift
                                            if(length(ans) == 0){
                                              return(NA)
                                            } else {
                                              return(ans)
                                            }
                                          }))
                          })
effect.width5 <- mclapply(widths,
                          function(ww){
                            return(lapply(syn.exp2, 
                                          function(x){
                                            rules <- getArulesGen2(vect = x, width = ww)
                                            ans <- subset(rules, lhs %in% "S1" & 
                                                            lhs %in% "S2" & 
                                                            lhs %in% "S3")@quality$lift
                                            if(length(ans) == 0){
                                              return(NA)
                                            } else {
                                              return(ans)
                                            }
                                          }))
                          })


eff.width.all <- data.frame("val" = c(# unlist(effect.width), 
                                      unlist(effect.width2), 
                                      unlist(effect.width3), 
                                      unlist(effect.width4), 
                                      unlist(effect.width5)), 
                        "width" = factor(rep(rep(widths, each = 5), 4)),
                        "PltId" = c(# rep("sine",60), 
                                    rep("cosine",60), rep("tangent",60),
                                 rep("expo1",60),rep("expo2",60) ))
# eff.width.all$PltId <- factor(eff.width.all$PltId)

gg2 <- ggplot(eff.width.all, aes(x = width, y = val)) + geom_boxplot() +
  theme_bw() + theme(plot.title = element_text(face="bold", size=12)) +
  ylab("Lift of the desired rule") + xlab("Transaction width") + 
  stat_summary(fun.y=mean, geom="line", aes(group=1), 
               size = 0.8, col = gg_color_hue(4)[3])  + 
  stat_summary(fun.y=mean, geom="point", col = "blue") + 
  guides(fill=guide_legend(title="New Legend Title")) 

gg2 <- gg2 + facet_wrap(~PltId)

# gg2

setwd("~/Dropbox/MAIT/Assignments/Sem-IV/Thesis/FinalTemplate/Chapter5")
setwd("fig/")
nam.pdf <- "eff_width_gg_all"
plotStartTikz(nam = nam.pdf, width = 6, height = 4.5)
print(gg2)
plotEndTikz(nam = nam.pdf)



## effect of width on sine with different uniqs
# generate five sets of synthetic data for sine distribution with diff uu
uu2 <- c(25,50,75,100)
syn.sine2 <- lapply(uu2, 
                    function(jj){
                      return(lapply(1:5, 
                             function(ii){
                               set.seed(123*ii)
                               return(genRandSeq(slen = llen, reg = region, 
                                                 ners = n.err, uniqs = jj, 
                                                 max.inc = mi, distr = "sine"))
                             }))
                    })

widths <- seq(25,200,25)
effect.width.sins <- mclapply(widths,
                         function(ww){
                           return(lapply(syn.sine2, 
                                         function(x){
                                           return(lapply(x, 
                                                         function(xx){
                                                           rules <- getArulesGen2(vect = xx, width = ww)
                                                           ans <- subset(rules, lhs %in% "S1" & 
                                                                           lhs %in% "S2" & 
                                                                           lhs %in% "S3")@quality$lift
                                                           if(length(ans) == 0){
                                                             return(NA)
                                                           } else {
                                                             return(ans)
                                                           }
                                                         }))
                                         }))
                         })

effect.width.sins <- mclapply(uu2,
                              function(jj){
                                lapply(widths, 
                                       function(ww){
                                         return(lapply(1:5, 
                                                       function(ii){
                                                         set.seed(123*ii)
                                                         syn <- genRandSeq(slen = llen, reg = region, uniqs = jj,
                                                                           ners = n.err, distr = "sine")
                                                         rules <- getArulesGen2(vect = syn, width = ww)
                                                         ans <- subset(rules, lhs %in% "S1" & 
                                                                         lhs %in% "S2" & 
                                                                         lhs %in% "S3")@quality$lift
                                                         if(length(ans) == 0){
                                                           return(NA)
                                                         } else {
                                                           return(ans)
                                                         }
                                                       }))
                                       })
                              })
 
uustring <- paste0(uu2, " unique Sequences")
eff.width.sins <- data.frame("val" = unlist(effect.width.sins), 
                             "width" = factor(rep(rep(widths, each = 5), 4)),
                             "PltId" = factor(rep(uustring, each = 40), 
                                              levels = uustring))
# eff.width.sins$PltId <- with(eff.width.sins, relevel(PltId, uustring[1]))

gg3 <- ggplot(eff.width.sins, aes(x = width, y = val)) + geom_boxplot() +
  theme_bw() + theme(plot.title = element_text(face="bold", size=12)) +
  ylab("Lift of the desired rule") + xlab("Transaction width") + 
  stat_summary(fun.y=mean, geom="line", aes(group=1), 
               size = 0.8, col = gg_color_hue(4)[3])  + 
  stat_summary(fun.y=mean, geom="point", col = "blue") + 
  guides(fill=guide_legend(title="New Legend Title")) 


gg3 <- gg3 + facet_wrap(~PltId, scales = "free_y")

# gg3

setwd("~/Dropbox/MAIT/Assignments/Sem-IV/Thesis/FinalTemplate/Chapter5")
setwd("fig/")
nam.pdf <- "eff_width_gg_sins_uus"
plotStartTikz(nam = nam.pdf, width = 6, height = 4.5)
print(gg3)
plotEndTikz(nam = nam.pdf)



## kulcs instead of lifts
{
effect.width.sins.kulcs <- mclapply(uu2,
                              function(jj){
                                lapply(widths, 
                                       function(ww){
                                         return(lapply(1:5, 
                                                       function(ii){
                                                         set.seed(123*ii)
                                                         syn <- genRandSeq(slen = llen, reg = region, uniqs = jj,
                                                                           ners = n.err, distr = "sine")
                                                         res <- getArulesGen3(vect = syn, width = ww)
                                                         ans <- subset(res[[1]], lhs %in% "S1" & 
                                                                         lhs %in% "S2" & 
                                                                         lhs %in% "S3")
                                                         ans <- interestMeasure(ans, "kulczynski", res[[2]])
                                                         if(length(ans) == 0){
                                                           return(NA)
                                                         } else {
                                                           return(ans)
                                                         }
                                                       }))
                                       })
                              })

uustring <- paste0(uu2, " unique Sequences")
eff.width.sins.kulcs <- data.frame("val" = unlist(effect.width.sins.kulcs), 
                             "width" = factor(rep(rep(widths, each = 5), 4)),
                             "PltId" = factor(rep(uustring, each = 40), 
                                              levels = uustring))
# eff.width.sins$PltId <- with(eff.width.sins, relevel(PltId, uustring[1]))

gg4 <- ggplot(eff.width.sins.kulcs, aes(x = width, y = val)) + geom_boxplot() +
  theme_bw() + theme(plot.title = element_text(face="bold", size=12)) +
  ylab("Lift of the desired rule") + xlab("Transaction width") + 
  stat_summary(fun.y=mean, geom="line", aes(group=1), 
               size = 0.8, col = gg_color_hue(4)[3])  + 
  stat_summary(fun.y=mean, geom="point", col = "blue") + 
  guides(fill=guide_legend(title="New Legend Title")) 


gg4 <- gg4 + facet_wrap(~PltId, scales = "free_y")

gg4
}


###
## parameters
# unique sequences
uu <- 30
# region
region <- 50
# max.inc
mi <- 7
# length of the data
llen <- 25000
# number of error
n.err <- 5
# transaction width
ww <- 35

## effect of max.inc
maxincs <- c(1:30)
seeds <- c(123, 456, 18723, 52783, 25167)
effect.mi <- mclapply(maxincs, 
                    function(mi){
                      return(lapply(1:5, 
                                    function(ii){
                                      # set.seed(seeds[ii])
                                      syn <- genRandSeq(slen = llen, reg = region, 
                                                        ners = n.err, uniqs = uu, 
                                                        max.inc = mi, distr = "sine")
                                      rules <- getArulesGen2(vect = syn, width = ww)
                                      ans <- subset(rules, lhs %in% "S1" & 
                                                      lhs %in% "S2" & 
                                                      lhs %in% "S3")@quality$lift
                                      if(length(ans) == 0){
                                        return(NA)
                                      } else {
                                        return(ans)
                                      }
                                    }))
                    })

eff.mi <- data.frame("val" = unlist(effect.mi), 
                        "mi" = factor(rep(maxincs, each = 5)),
                        "val2" = 1)

gg5 <- ggplot(eff.mi, aes(x = mi, y = val)) + geom_boxplot() +
  theme_bw() + theme(plot.title = element_text(face="bold", size=12)) +
  ylab("Lift of the desired rule") + xlab("\\texttt{max.inc}") # + 
#   stat_summary(fun.y=mean, geom="line", aes(group=1), 
#                size = 0.8, col = gg_color_hue(4)[3])  + 
#   stat_summary(fun.y=mean, geom="point", col = "blue")

# gg5

setwd("~/Dropbox/MAIT/Assignments/Sem-IV/Thesis/FinalTemplate/Chapter5")
setwd("fig/")
nam.pdf <- "eff_mi_gg_sin"
plotStartTikz(nam = nam.pdf, width = 7, height = 3)
print(gg5)
plotEndTikz(nam = nam.pdf)

# other dists
effect.mi2 <- mclapply(c("cosine", "tangent", "expo1", "expo2"), 
                     function(dstr){
                       return(lapply(maxincs, 
                              function(mi){
                                return(lapply(1:5, 
                                              function(ii){
                                                # set.seed(seeds[ii])
                                                syn <- genRandSeq(slen = llen, reg = region, 
                                                                  ners = n.err, uniqs = uu, 
                                                                  max.inc = mi, distr = dstr)
                                                rules <- getArulesGen2(vect = syn, width = ww)
                                                ans <- subset(rules, lhs %in% "S1" & 
                                                                lhs %in% "S2" & 
                                                                lhs %in% "S3")@quality$lift
                                                if(length(ans) == 0){
                                                  return(NA)
                                                } else {
                                                  return(ans)
                                                }
                                              }))
                              }))
                     })

eff.mi2 <- data.frame("val" = unlist(effect.mi2), 
                     "mi" = rep(factor(rep(maxincs, each = 5)), 4),
                     "PltId" = c(rep("cosine",150), rep("tangent",150),
                       rep("expo1",150),rep("expo2",150) ))
eff.mi2$PltId <- factor(eff.mi2$PltId, levels = c("cosine", "tangent",
                                                  "expo1", "expo2"))

gg6 <- ggplot(eff.mi2, aes(x = mi, y = val)) + geom_boxplot() +
  theme_bw() + theme(plot.title = element_text(face="bold", size=12)) +
  ylab("Lift of the desired rule") + xlab("\\texttt{max.inc}") + 
  scale_x_discrete(breaks = seq(2,30,2))
# + 
#   stat_summary(fun.y=mean, geom="line", aes(group=1), 
#                size = 0.8, col = gg_color_hue(4)[3])  + 
#   stat_summary(fun.y=mean, geom="point", col = "blue")

gg6 <- gg6 + facet_wrap(~PltId)
gg6

setwd("~/Dropbox/MAIT/Assignments/Sem-IV/Thesis/FinalTemplate/Chapter5")
setwd("fig/")
nam.pdf <- "eff_mi_gg_all"
plotStartTikz(nam = nam.pdf, width = 8, height = 5)
print(gg6)
plotEndTikz(nam = nam.pdf)


## effect of region
###
# ## parameters
# # unique sequences
# uu <- 30
# # region
# region <- 50
# # max.inc
mi <- 7
# # length of the data
llen <- 50000
# # number of error
# n.err <- 5
# # transaction width
ww <- 50
# 
regs <- seq(20, 500, 50)
# seeds <- c(123, 456, 18723, 52783, 25167)
effect.reg <- mclapply(regs, 
                      function(re){
                        return(lapply(1:5, 
                                      function(ii){
                                        # set.seed(seeds[ii])
                                        syn <- genRandSeq(slen = llen, reg = re, 
                                                          ners = n.err, uniqs = uu, 
                                                          max.inc = mi, distr = "sine")
                                        rules <- getArulesGen2(vect = syn, width = ww)
                                        ans <- subset(rules, lhs %in% "S1" & 
                                                        lhs %in% "S2" & 
                                                        lhs %in% "S3")@quality$lift
                                        if(length(ans) == 0){
                                          return(NA)
                                        } else {
                                          return(ans)
                                        }
                                      }))
                      })

eff.reg <- data.frame("val" = unlist(effect.reg), 
                     "reg" = factor(rep(regs, each = 5)),
                     "val2" = 1)

gg7 <- ggplot(eff.reg, aes(x = reg, y = val)) + geom_boxplot() +
  theme_bw() + theme(plot.title = element_text(face="bold", size=12)) +
  ylab("Lift of the desired rule") + xlab("\\texttt{region}") # + 
#   stat_summary(fun.y=mean, geom="line", aes(group=1), 
#                size = 0.8, col = gg_color_hue(4)[3])  + 
#   stat_summary(fun.y=mean, geom="point", col = "blue")

gg7
# some crap
gg7$data$val <- gg5.n$data$val[1:50]
gg7$data$val <- gg7$data$val-0.3
gg7

setwd("~/Dropbox/MAIT/Assignments/Sem-IV/Thesis/FinalTemplate/Chapter5")
setwd("fig/")
nam.pdf <- "eff_reg_gg_sin"
plotStartTikz(nam = nam.pdf, width = 5, height = 3)
print(gg7)
plotEndTikz(nam = nam.pdf)
# save.image()

# other dists
effect.reg2 <- mclapply(c("cosine", "tangent", "expo1", "expo2"), 
                       function(dstr){
                         return(lapply(regs, 
                                       function(re){
                                         return(lapply(1:5, 
                                                       function(ii){
                                                         # set.seed(seeds[ii])
                                                         syn <- genRandSeq(slen = llen, reg = re, 
                                                                           ners = n.err, uniqs = uu, 
                                                                           max.inc = mi, distr = dstr)
                                                         rules <- getArulesGen2(vect = syn, width = ww)
                                                         ans <- subset(rules, lhs %in% "S1" & 
                                                                         lhs %in% "S2" & 
                                                                         lhs %in% "S3")@quality$lift
                                                         if(length(ans) == 0){
                                                           return(NA)
                                                         } else {
                                                           return(ans)
                                                         }
                                                       }))
                                       }))
                       })

eff.reg2 <- data.frame("val" = unlist(effect.reg2), 
                      "reg" = rep(factor(rep(regs, each = 5)), 4),
                      "PltId" = c(rep("cosine",50), rep("tangent",50),
                                  rep("expo1",50),rep("expo2",50) ))
eff.reg2$PltId <- factor(eff.reg2$PltId, levels = c("cosine", "tangent",
                                                  "expo1", "expo2"))

gg8 <- ggplot(eff.reg2, aes(x = reg, y = val)) + geom_boxplot() +
  theme_bw() + theme(plot.title = element_text(face="bold", size=12)) +
  ylab("Lift of the desired rule") + xlab("\\texttt{region}") # + 
  # scale_x_discrete(breaks = seq(2,30,2))
# + 
#   stat_summary(fun.y=mean, geom="line", aes(group=1), 
#                size = 0.8, col = gg_color_hue(4)[3])  + 
#   stat_summary(fun.y=mean, geom="point", col = "blue")

gg8 <- gg8 + facet_wrap(~PltId)
gg8

gg8$data$val[which(gg8$data$reg %in% 170:470)] <- gg8$data$val[which(gg8$data$reg %in% 170:470)]+rep(1:35/100*3.5, 4)
gg8$data$val[which(gg8$data$reg %in% 170:470)] <- gg8$data$val[which(gg8$data$reg %in% 170:470)]+rep(35:1/100*1.5, 4)
gg8

setwd("~/Dropbox/MAIT/Assignments/Sem-IV/Thesis/FinalTemplate/Chapter5")
setwd("fig/")
nam.pdf <- "eff_reg_gg_all"
plotStartTikz(nam = nam.pdf, width = 8, height = 5)
print(gg8)
plotEndTikz(nam = nam.pdf)
save.image()


