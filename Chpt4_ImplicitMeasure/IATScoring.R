######### IAT Scoring Algorithms ###########
# Author: Joanna Moody
# Date: Sunday, July 23, 2017

library(car)
# Set the path to the IAT folder here (be sure to include trailing slash)
# on Linux (use a single forward slash) on Windows (use double forward slashes)
base.dir = "/Users/jcmoody/Dropbox (MIT)/Joanna_Jintai_Jinhua/CarPride/Source Code/"

# File delimiter = '/' for Unix/Max, '//' for Windows
fd = '/'

# Set the template you want to analyze here
template.name = "Travel Mode"
output.dir = paste(base.dir,"templates",fd,template.name,fd,"output",fd,sep="")

setwd(output.dir)
output.files = list.files()
# Each raw data (txt) file has 240 rows with six entries (comma-separated): 

data.list = lapply(output.files, read.table, sep = ",")
#This creates a list of dataframes (one for each of the 2061 respondents), with 240 rows (trails) x 6 columns
# "V1=Block#, V2=Trial#, V3=Concept or Attribute, V4=Image or Word index, V5=Error?, V6=Response Time (ms)"



#### D-SCORE ALGORITHM FROM GREENWALD ET AL (2003) ####
## Step 1. Use data from Blocks 3, 4, 6, and 7 only
## Step 2. Eliminate trials with latencies 10,000 ms or higher; eliminate subjects for whom more than 10% of trials have latency less than 300 ms
## Step 3. Compute one pooled SD for all trials in B3 & B6; another for B4 & B7
## Step 4. Compute mean latencies for responses in each of blocks 3, 4, 6, and 7
## Step 5. Compute the mean differences (Mean_b6 - Mean_b3) and (Mean_b7 = Mean_b4)
## Step 6. Divide each difference score by its associated "inclusive" standard deviation.
## Step 7. D = equal-weight average of the two resulting ratios.

data.D <- data.frame(matrix(ncol=15,nrow=length(data.list)))
names(data.D) <- c("ID", "Date", "Treatment", "Fast", "mean.3", "mean.4", "mean.6", "mean.7", "sd.36", "sd.47", "meandiff.63", "meandiff.74", "d63", "d74", "D")

for(i in 1:length(data.list))
{
  #File Names are constructed as follows: Travel Mode-5138A0EWI8-2016-07-08-16-17-0.txt
  # Travel Mode - 10-character UNIQUE ID - Date (2016-07-08) - - Treatment.txt
  filename = strsplit(output.files[i],'-')
  id = filename[[1]][2]
  iat.date = paste(filename[[1]][4],"-",filename[[1]][5],"-",filename[[1]][3]," ",filename[[1]][6],":",substr(filename[[1]][7],1,2),sep="")
  iat.treat = filename[[1]][8]
  
  X <- data.list[[i]]
  #D-score produced by this algorithm will be POSITIVE for CAR PRIDE/BUS SHAME and negative for bus pride/car shame
  if (iat.treat == "0.txt"){
    X$V1 <- recode(X$V1, "0=1; 1=2; 2=6; 3=7; 4=5; 5=3; 6=4")
  }
  else{
    X$V1 <- recode(X$V1, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7") 
  }
  
  X <- X[which(X$V1%in%c(3,4,6,7)), ]  #Delete all rows in the element dataframes that have V1=1,2, or 5 
  
  X <- X[which(X$V6<10000), ] # Delete trials with latency > 10s
  #Mark subjects for whom more than 10% of trials have latency less than 300 ms
      if (length(which(X$V6<300))>(0.10*length(X$V6))) {
          fast <- 1
      }
      else{
          fast <- 0
      }
      
  sd.36 <- sd(X[ which(X$V1%in%c(3,6)), 6])
  sd.47 <- sd(X[ which(X$V1%in%c(4,7)), 6])
  
  mean.3 <- mean(X[ which(X$V1==3), 6])
  mean.4 <- mean(X[ which(X$V1==4), 6])
  mean.6 <- mean(X[ which(X$V1==6), 6])
  mean.7 <- mean(X[ which(X$V1==7), 6])

  meandiff.63 = mean.6 - mean.3
  meandiff.74 = mean.7 - mean.4
      
  d63 = meandiff.63/sd.36
  d74 = meandiff.74/sd.47
  
  D=((d74+d63)/2)
  
  data.D[i,] = c(id, iat.date, iat.treat, fast, mean.3, mean.4, mean.6, mean.7, sd.36, sd.47, meandiff.63, meandiff.74, d63, d74, D)
}

length(which(data.D$Fast == 1)) ##Throws out 183 responses out of 2061
data.D[which(data.D$Fast==1),15] <- NA

write.csv(data.D, "20170724_Dscore.csv")



#### DW-SCORE ALGORITHM FROM RICHETIN ET AL (2015) ####
## Step 1. Use data from Blocks 3, 4, 6, and 7 only
## Step 2. Eliminate trials with latencies 10,000 ms or higher (both correct and error)
## Step 3. Perform Statistical Winsorizing: replace the 10% fastest and slowest latencies by the last untrimmed latencies for both error and correct responses 
## Step 4. Compute mean latencies: Mean_(b3 and b4) and Mean_(b6 and b7)
           # Usually this is done on 20 practice trials (in b3 and b6) and 40 test trials (b4 and b7)
           # We have 40 trials in both practice and test blocks, so we could consider throwing out the first 20 trials in b3 and b6 to be more consistent with literatyre
## Step 5. Compute one pooled SD for all trials in b3, b4, b6, and b7
## Step 6. Compute the mean difference: Mean_(b6 and b7) - Mean_(b3 and b4)
## Step 7. DW = Divide the mean difference by the pooled SD

data.dw <- data.frame(matrix(ncol=7,nrow=length(data.list)))
names(data.dw) <- c("ID", "Date", "Treatment", "mean.34", "mean.67", "sd.all", "DW")

for(i in 1:length(data.list))
{
  #File Names are constructed as follows: Travel Mode-5138A0EWI8-2016-07-08-16-17-0.txt
  # Travel Mode - 10-character UNIQUE ID - Date (2016-07-08) - - Treatment.txt
  filename = strsplit(output.files[i],'-')
  id = filename[[1]][2]
  iat.date = paste(filename[[1]][4],"-",filename[[1]][5],"-",filename[[1]][3]," ",filename[[1]][6],":",substr(filename[[1]][7],1,2),sep="")
  iat.treat = filename[[1]][8]
  
  X <- data.list[[i]]
  #D-score produced by this algorithm will be POSITIVE for CAR PRIDE/BUS SHAME and negative for bus pride/car shame
  if (iat.treat == "0.txt"){
    X$V1 <- recode(X$V1, "0=1; 1=2; 2=6; 3=7; 4=5; 5=3; 6=4")
  }
  else{
    X$V1 <- recode(X$V1, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7") 
  }
  
  X <- X[which(X$V1%in%c(3,4,6,7)), ]  #Delete all rows in the element dataframes that have V1=1,2, or 5 
  X <- X[which(X$V6<10000), ]  # Delete trials with latency > 10s
  
  ## Statistical Winsorizing
  num.trials <- length(X$V6)
  ten.pctile <- 0.10 * num.trials
  ninety.pctile <- 0.90 * num.trials
  X <- X[order(X$V6),]
  
  for (j in 1:num.trials){
    if (j < ten.pctile){
      X[j,6] <- X[ten.pctile,6]
    }
    else if (j > ninety.pctile){
      X[j,6] <- X[ninety.pctile,6]
    }
  }
  
  mean.34 <- mean(X[which(X$V1%in%c(3,4)), 6])
  mean.67 <- mean(X[which(X$V1%in%c(6,7)), 6])
  sd.all <- sd(X$V6)
  
  DW <- (mean.67 - mean.34)/sd.all
  
  data.dw[i,] = c(id, iat.date, iat.treat, mean.34, mean.67, sd.all, DW)
}
write.csv(data.dw, "20170724_DWscore.csv")

