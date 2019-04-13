# Libraries

library(tabulizer)

library(tidyverse)

library(DataExplorer)




setwd("./District 7")



salaries <- extract_tables("http://ecusd7.org/wp-content/uploads/bsk-pdf-manager/2019/01/Certified-Administrator-and-Teacher-Compensation-Report.pdf")





Name <- c(salaries[[1]][,1],salaries[[2]][,1],salaries[[3]][,1],salaries[[4]][,1],salaries[[5]][,1],
          
          salaries[[6]][,1],salaries[[7]][,1],salaries[[8]][,1],salaries[[9]][,1],salaries[[10]][,1],
          
          salaries[[11]][,1],salaries[[12]][,1],salaries[[13]][,1])



Position <- c(salaries[[1]][,2],salaries[[2]][,2],salaries[[3]][,2],salaries[[4]][,2],salaries[[5]][,2],
              
              salaries[[6]][,2],salaries[[7]][,2],salaries[[8]][,2],salaries[[9]][,2],salaries[[10]][,2],
              
              salaries[[11]][,2],salaries[[12]][,2],salaries[[13]][,2])



BaseSalary <- c(salaries[[1]][,3],salaries[[2]][,3],salaries[[3]][,3],salaries[[4]][,3],salaries[[5]][,3],
                
                salaries[[6]][,3],salaries[[7]][,3],salaries[[8]][,3],salaries[[9]][,3],salaries[[10]][,3],
                
                salaries[[11]][,3],salaries[[12]][,3],salaries[[13]][,3])



FTE <- c(salaries[[1]][,4],salaries[[2]][,4],salaries[[3]][,4],salaries[[4]][,4],salaries[[5]][,4],
         
         salaries[[6]][,4],salaries[[7]][,4],salaries[[8]][,4],salaries[[9]][,4],salaries[[10]][,4],
         
         salaries[[11]][,4],salaries[[12]][,4],salaries[[13]][,4])



Retirement <- c(salaries[[1]][,9],salaries[[2]][,9],salaries[[3]][,9],salaries[[4]][,9],salaries[[5]][,9],
                
                salaries[[6]][,9],salaries[[7]][,9],salaries[[8]][,9],salaries[[9]][,9],salaries[[10]][,9],
                
                salaries[[11]][,9],salaries[[12]][,9],salaries[[13]][,9])



OtherBenefits <- c(salaries[[1]][,10],salaries[[2]][,10],salaries[[3]][,10],salaries[[4]][,10],salaries[[5]][,10],
                   
                   salaries[[6]][,10],salaries[[7]][,10],salaries[[8]][,10],salaries[[9]][,10],salaries[[10]][,10],
                   
                   salaries[[11]][,10],salaries[[12]][,10],salaries[[13]][,10])



df <- data.frame(Name, Position, BaseSalary, FTE, Retirement, OtherBenefits)

df <- subset(df, df$Name != "ame")



df$Name <- as.character(df$Name)

df$BaseSalary <- as.numeric(gsub('[$,]','',df$BaseSalary))

df$FTE <- as.numeric(gsub('[$,]','',df$FTE))

df$Retirement <- as.numeric(gsub('[$,]','',df$Retirement))

df$OtherBenefits <- as.numeric(gsub('[$,]','',df$OtherBenefits))

df$ROBprop <- (df$Retirement + df$OtherBenefits) / df$BaseSalary

df$AdjSalary <- df$BaseSalary / df$FTE



df_sub40 <- subset(df, df$AdjSalary < 40000)

df_sub40$BelowMin <- 40000 - df_sub40$AdjSalary

maxSalary <- sum(df_sub40$BelowMin)

maxROB <- maxSalary * mean(df_sub40$ROBprop, na.rm = TRUE)

maxTotal <- maxSalary + maxROB



df_sub40FTE <- subset(df, (df$BaseSalary < 40000) & (df$FTE > 0.99))

df_sub40FTE$BelowMin <- 40000 - df_sub40FTE$BaseSalary

minSalary <- sum(df_sub40FTE$BelowMin)

minROB <- minSalary * mean(df_sub40FTE$ROBprop, na.rm = TRUE)

minTotal <- minSalary + minROB



df_sub40FTE$percIncrease <- (((df_sub40FTE$BelowMin + df_sub40FTE$BaseSalary) - df_sub40FTE$BaseSalary) / df_sub40FTE$BaseSalary) * 100



plot_histogram(df_sub40FTE$BelowMin)