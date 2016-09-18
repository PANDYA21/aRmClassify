#############################################################################
## User-Defined-Functions for state-transition / sequence analysis 
## Master Thesis: Bhaumik Pandya
## Part - 2
#############################################################################


###### Remove flame sensor test toggling
###### Remove flame sensor test toggling - generalized
remTogg <- function(Seq, st1, st2){
  require(stringr)
  
  ### make a regex pattern to locate, replace and sum the durations of flame sensor-
  ##  test toggling states
  
  ## ref
  ## pat <- "[G|D] [[:digit:]]* ([G|D] [[:digit:]]* ?)+"
  
  if(length(grep("[[:punct:]]", st1)) == 0 & length(grep("[[:punct:]]", st2)) == 0){
    st1.rep <- st1
    st2.rep <- st2
    pat <- strcat(c("[", st1, "|", st2, "] [[:digit:]]* ([", st1, "|", st2, "] [[:digit:]]* ?)+"))
  }else{
    st1.rep <- st1
    st2.rep <- st2
    st11 <- unlist(strsplit(st1, ""))
    st1 <- st11[1]
    pun1 <- st11[2]
    st11 <- unlist(strsplit(st2, ""))
    st2 <- st11[1]
    pun2 <- st11[2]
    rm(st11)
    if(is.na(pun1)){
      pun1 <- ""
    }else{
      pun1 <- strcat(c("\\", pun1))
    }
    
    if(is.na(pun2)){
      pun2 <- ""
    }else{
      pun2 <- strcat(c("\\", pun2))
    }
    
    if(pun1 == pun2){
      pat <- strcat(c("[", st1, "|", st2, "]", pun1, " [[:digit:]]* ([", 
                      st1, "|", st2, "]", pun2, " [[:digit:]]* ?)+"))
    }else{
      # pat <- strcat(c("[", st1, pun1, "|", st2, pun2, "]", " [[:digit:]]* ([", 
      #                st1, pun1, "|", st2, pun2, "]", " [[:digit:]]* ?)+"))
      pat <- strcat(c("(", st1, pun1, "|", st2, pun2, ")", " [[:digit:]]* ((", 
                      st1, pun1, "|", st2, pun2, ")", " [[:digit:]]* ?)+"))
    }
  }
  
  newstr <- gsub(pat, "&&& ", Seq) 
  exstr <- unlist(str_extract_all(Seq, pat))
  durs <- unlist(lapply(exstr, function(xx){sum(as.numeric(as.character(str2num2(xx, " "))))}))
  for(dd in durs)
  {
    newstr <- str_replace(newstr, "&&&", strcat(c(st1.rep, " ", dd)))
  }
  return(newstr)
}


# 
# ###### Remove flame sensor test toggling - generalized
# ###### Older version
# remTogg <- function(Seq, st1, st2){
#   require(stringr)
#   
#   pat <- strcat(c(st1, " [[:digit:]]* ", st2, " [[:digit:]]*"))
#   if(length(grep(pat, Seq)) == 0){
#     return(Seq)
#   }else{
#     pat <- strcat(c(" ", st1, " [[:digit:]]* ", st2, " [[:digit:]]*", "| ", 
#                     st1, " [[:digit:]]* ", st2, " [[:digit:]]*", "| ",
#                     st1, " [[:digit:]]* ", st2, " [[:digit:]]*", "| ",
#                     st1, " [[:digit:]]* ", st2, " [[:digit:]]*"))
#     locs <- str_locate_all(Seq, pat)
#     locs <- data.frame(locs[[1]])
#     dif <- numeric(0)
#     for(ii in 2:dim(locs)[1])
#     {
#       dif <- c(dif, locs$start[ii] - locs$end[ii-1])
#     }
#     split.here <- locs$end[which(dif > 1)]
#     if(length(split.here) == 0){
#       Seq <- removeTogg(Seq, st1, st2)
#       return(Seq)
#     }else{
#       s1 <- substr(Seq, 1, split.here[1])
#       s2 <- substr(Seq, split.here[1]+2, nchar(Seq))
#       
#       if(length(split.here) > 1){
#         s2 <- remTogg(s2, st1, st2)
#       }
#       
#       if(substr(s2, 1, 1) == st1){
#         pat1 <- strcat(c(st1 ," [[:digit:]]*"))
#         s1 <- strcat(c(s1, str_extract(s2, pat1)))
#         pat2 <- strcat(c(st1, " [[:digit:]]* "))
#         s2 <- str_replace(s2, pat2, "")
#       }
#       Seq <- strcat(c(removeTogg(s1, st1, st2), " ", removeTogg(s2, st1, st2)))
#       return(Seq)
#     }
#   }
# }
# 
# 
# ## removeTogg - internal for remTogg
# removeTogg <- function(Seq, st1, st2){
#   loc.G <- which(num2str2(Seq, " ") == st1)
#   loc.Gs <- which(num2str2(Seq, " ") == strcat(c(st1, " ")))
#   loc.G <- sort(c(loc.G, loc.Gs))
#   loc.D <- which(num2str2(Seq, " ") == st2)
#   loc.Ds <- which(num2str2(Seq, " ") == strcat(c(st2, " ")))
#   loc.D <- sort(c(loc.D, loc.Ds))
#   rm(list = c("loc.Gs", "loc.Ds"))
#   if(max(loc.G) >= max(loc.D)){
#     dur.G <- sum(as.numeric(as.character(str2num2(Seq, " ")[loc.G[1]:loc.G[length(loc.G)]])), na.rm = T)
#   }else{
#     dur.G <- sum(as.numeric(as.character(str2num2(Seq, " ")[loc.G[1]:loc.D[length(loc.D)]])), na.rm = T)
#   }
#   
#   pat1 <- strcat(c(st1, " [[:digit:]]* ", st2, " [[:digit:]]*"))
#   Seq <- gsub(pat1, st1, Seq)
#   
#   pat2 <- strcat(c(st1, " [[:digit:]]+"))
#   Seq <- gsub(pat2, st1, Seq)
#   
#   pat3 <- strcat(c(st1, " ", st1, " "))
#   Seq <- gsub(pat3, st1, Seq)
#   
#   pat4 <- strcat(c(st1, " ", st1))
#   Seq <- gsub(pat4, st1, Seq)
#   
#   pat5 <- strcat(c(st1, st1, "*"))
#   Seq <- gsub(pat5, st1, Seq)
#   
#   if(length(grep("[[:punct:]]", st1)) == 0){
#     loc.G <- which(unlist(strsplit(Seq, "")) == st1)
#   }else{
#     loc.G <- (2 * which(unlist(strsplit(Seq, " ")) == st1)) + 1
#   }
#   
#   Seq <- strcat(c(substr(Seq, 1, loc.G), " ", dur.G, substr(Seq, loc.G+1, nchar(Seq))))
#   
#   return(Seq)
# }
# ####
# 


# Very old method
# #### 1. Remove CH time flame sensor flame test toggling (SP reached)
# remGD <- function(Seq){
#   library(stringr)
#   # locs <- str_locate_all(strcat(unlist(strsplit(num2str(Seq), " "))), "GD")
#   locs <- str_locate_all(Seq, " G [[:digit:]]* D [[:digit:]]*")
#   locs <- data.frame(locs[[1]])
#   dif <- numeric(0)
#   for(ii in 2:dim(locs)[1])
#   {
#     dif <- c(dif, locs$start[ii] - locs$end[ii-1])
#   }
#   split.here <- locs$end[which(dif > 1)]
#   if(length(split.here) == 0){
#     Seq <- removeGD(Seq)
#     return(Seq)
#   }else{
#     s1 <- substr(Seq, 1, split.here[1])
#     s2 <- substr(Seq, split.here[1]+1, nchar(Seq))
#     if(substr(s2, 1, 1) == "G"){
#       s1 <- strcat(c(s1, str_extract(s2, "G [[:digit:]]*")))
#       s2 <- str_replace(s2, "G [[:digit:]]* ", "")
#     }
#     Seq <- strcat(c(removeGD(s1), " ", removeGD(s2)))
#     return(Seq)
#   }
# }
# 
# ## remove GD - internal - for "remGD"
# removeGD <- function(Seq){
#   loc.G <- which(num2str2(Seq, " ") == "G")
#   loc.Gs <- which(num2str2(Seq, " ") == "G ")
#   loc.G <- sort(c(loc.G, loc.Gs))
#   loc.D <- which(num2str2(Seq, " ") == "D")
#   loc.Ds <- which(num2str2(Seq, " ") == "D ")
#   loc.D <- sort(c(loc.D, loc.Ds))
#   rm(list = c("loc.Gs", "loc.Ds"))
#   if(max(loc.G) >= max(loc.D)){
#     dur.G <- sum(as.numeric(as.character(str2num2(Seq, " ")[loc.G[1]:loc.G[length(loc.G)]])))
#   }else{
#     dur.G <- sum(as.numeric(as.character(str2num2(Seq, " ")[loc.G[1]:loc.D[length(loc.D)]])))
#   }
#   
#   Seq <- gsub("G [[:digit:]]* D [[:digit:]]*",
#               "G",
#               Seq)
#   
#   Seq <- gsub("G [[:digit:]]+",
#               "G",
#               Seq)
#   
#   Seq <- gsub("G G ",
#               "G",
#               Seq)
#   
#   Seq <- gsub("G G",
#               "G",
#               Seq)
#   
#   Seq <- gsub("GG*",
#               "G",
#               Seq)
#   
#   loc.G <- which(unlist(strsplit(Seq, "")) == "G")
#   Seq <- strcat(c(substr(Seq, 1, loc.G), " ", dur.G, substr(Seq, loc.G+1, nchar(Seq))))
#   
#   return(Seq)
# }
# ####


#### 1. Remove CH time flame sensor flame test toggling (SP not reached)
####    Replace "G [[:digit:]] D [[:digit:]]" by "G [[:sum_digit]]"
rem_GD <- function(Seq){
  return(remTogg(Seq, "G", "D"))
}
####


#### 2. Remove CH time flame sensor flame test toggling (SP reached)
####    Replace "H [[:digit:]] N [[:digit:]]" by "H [[:sum_digit]]"
rem_HN <- function(Seq){
  return(remTogg(Seq, "H", "N"))
}
####


#### 3. Remove DHW time flame sensor flame test toggling (SP not reached)
####    Replace "d [[:digit:]] a [[:digit:]]" by "d [[:sum_digit]]"
rem_da <- function(Seq){
  return(remTogg(Seq, "d", "a"))
}
####


#### 4. Remove DHW time flame sensor flame test toggling (SP reached)
####    Replace "e [[:digit:]] h [[:digit:]]" by "e [[:sum_digit]]"
rem_eh <- function(Seq){
  return(remTogg(Seq, "e", "h"))
}
####


#### 5. Remove CH & DHW time flame sensor flame test toggling (SP not reached)
####    Replace "F- [[:digit:]] E- [[:digit:]]" by "F- [[:sum_digit]]"
rem_FmEm <- function(Seq){
  return(remTogg(Seq, "F-", "E-"))
}
####


#### 6. Remove CH & DHW time flame sensor flame test toggling (SP reached)
####    Replace "V- [[:digit:]] U- [[:digit:]]" by "V- [[:sum_digit]]"
rem_VmUm <- function(Seq){
  return(remTogg(Seq, "V-", "U-"))
}
####


#### Fan flag ON in all above
#### Fan flag OFF from here


#### 7. Remove CH time flame sensor flame test toggling (SP reached)
####    Replace "A+ [[:digit:]] z [[:digit:]]" by "A+ [[:sum_digit]]"
rem_Apz <- function(Seq){
  return(remTogg(Seq, "A+", "z"))
}
####


#### 8. Remove CH time flame sensor flame test toggling (SP not reached)
####    Replace "K+ [[:digit:]] J+ [[:digit:]]" by "K+ [[:sum_digit]]"
rem_KpJp <- function(Seq){
  return(remTogg(Seq, "K+", "J+"))
}
####


#### 9. Remove DHW time flame sensor flame test toggling (SP not reached)
####    Replace "U [[:digit:]] T [[:digit:]]" by "U [[:sum_digit]]"
rem_UT <- function(Seq){
  return(remTogg(Seq, "U", "T"))
}
####


#### 10. Remove DHW time flame sensor flame test toggling (SP reached)
####    Replace "q [[:digit:]] h [[:digit:]]" by "p [[:sum_digit]]"
rem_qp <- function(Seq){
  return(remTogg(Seq, "q", "p"))
}
####


#### 11. Remove CH & DHW time flame sensor flame test toggling (SP not reached)
####    Replace "x+ [[:digit:]] w+ [[:digit:]]" by "x+ [[:sum_digit]]"
rem_xpwp <- function(Seq){
  return(remTogg(Seq, "x+", "w+"))
}
####


#### 12. Remove CH & DHW time flame sensor flame test toggling (SP reached)
####    Replace "N- [[:digit:]] M- [[:digit:]]" by "N- [[:sum_digit]]"
rem_NmMm <- function(Seq){
  return(remTogg(Seq, "N-", "M-"))
}
####


#### *. Remove all of above altogether
rem_all <- function(Seq){
  return(rem_NmMm(rem_xpwp(rem_qp(rem_UT(rem_KpJp(rem_Apz(rem_VmUm(rem_FmEm(rem_da(rem_eh(rem_HN(rem_GD(Seq)))))))))))))
}
####

#############################################################################