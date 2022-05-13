library(data.table)
library(fmsb)

# ------------------------
# Calculating IRD for MPC papers
# ------------------------

IRD <- function(data){
  refdata <- subset(data, data$state == "Baseline")
  comp.dat <- merge(data, refdata, by= "age",all.x = TRUE)
  comp.dat$state.x <- relevel(as.factor(comp.dat$state.x), ref = "Baseline")
  comp.dat <- comp.dat[order(factor(comp.dat$state.x)),]
  
  mat <- matrix(NA,nrow = nrow(comp.dat), ncol = 3)
  for (i in 1:nrow(comp.dat)){
    RD <- ratedifference(comp.dat[i,'aesi.x'], comp.dat[i,'aesi'], comp.dat[i,'fu.x'], comp.dat[i,'fu.y'], CRC = FALSE, conf.level = 0.95)
    mat[i,] <- round(c(RD$estimate, RD$conf.int),digits = 4)
  }
  mat[which(comp.dat$state.x == "Baseline"),] <- c(0,0,0)
  colnames(mat) <- c("RD","95LL","95UL")  
  rownames(mat) <- comp.dat$state.x 
  return(mat)
}

Mat.Out <- cbind(c(rep("Dose12-Back",nrow(df_all12)),rep("Dose1-Back",nrow(df_all1)),rep("Dose2-Back", nrow(df_all2))),
                 rbind(IRD(df_all12),IRD(df_all1), IRD(df_all2)))
write.csv(Mat.Out, "MatOut.csv")

## use th