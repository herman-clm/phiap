
script.dir <- dirname(sys.frame(1)$ofile)
source(paste(script.dir, "R_fxns.R", sep="/"))

library(RUnit)


#########################################
##       true_false_NA in R_fxns.R     ##
#########################################
## true_false_NA in R_fxns.R
true_false_NA.test <- function(){
  aldo1 <- c(NA, 20, 17, 20)
  pra1 <- c(2, 2, NA, 0.5)
  
  aldo2 <- c(NA, NA)
  pra2 <- c(NA, NA)
  
  aldo3 <- c(NA, 10)
  pra3 <- c(.5, NA)
  
  aldo4 <- c(NA, 10, 17, 12)
  pra4 <- c(2, 2, NA, 0.5)
  
  
  checkEquals(true_false_NA(aldo1, pra1, "ALL_Strict"), TRUE)
  checkEquals(true_false_NA(aldo2, pra2, "ALL_Strict"), NA)
  checkEquals(true_false_NA(aldo3, pra3, "ALL_Strict"), TRUE)
  checkEquals(true_false_NA(aldo3, pra3, "Aldo_Strict"), FALSE)
  checkEquals(true_false_NA(aldo3, pra3, "PRA_Strict"), TRUE)
  checkEquals(true_false_NA(aldo4, pra4, "ALL_Strict"), FALSE)
  checkEquals(true_false_NA(aldo4, pra4, "Aldo_Strict"), TRUE)
  checkEquals(true_false_NA(aldo4, pra4, "PRA_Strict"), TRUE)
  checkEquals(true_false_NA(aldo4, pra4, "Aldo_Lax"), TRUE)
  checkEquals(true_false_NA(aldo4, pra4, "ALL_Lax"), TRUE)
}


true_false_NA.test()

