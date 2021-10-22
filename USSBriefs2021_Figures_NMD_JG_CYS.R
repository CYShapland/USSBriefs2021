##############################################################################################################
# Code for "The USS Trustee's risky strategy" 2021
# Authors: Neil M Davies, Jackie Grant, Chin Yang Shapland
# Last updated: 19/10/21
###############################################################################################################

### Note ###
# (1) This code is converted from Matlab code by David Miles and James Sefton 2021 (M&S)
# (2) For Figure 5, go to the last section.

### Directories and packages ###

library(matconv)
library(zoo)
library(ggplot2)
library(dplyr)
library(reshape2)
library(readxl)

### Read data ###

USS_CashFlows <- read_excel("USS_CashFlows.xlsx", sheet = "QuickCalcs")
Cashflows<-as.data.frame(USS_CashFlows["Expected cash flows (£bn) in relation to benefits accrued at 31 March 2020"])
CPI <- USS_CashFlows["CPI Index"]
EXtraflows <- USS_CashFlows["Expected cash flows (£bn) in relation to benefits projected to be accrued in 2020/21"]
RealSpot <- USS_CashFlows["Spot Real Yields"]

periods <- nrow(USS_CashFlows) #periods in model

##################################################
## Parameter settings for different figures
##################################################

### Figure 1 ###
#Figure 1 of our manuscript replicates Figure 2 from M&S

rpiadj <- 0 

#Assume Arithmetric mean return is 4.5# and the std dev of log returns is 16.6#
# Average arithmetic rate of return on risky asssets (equities)
mew <- 0.045 

# st deviation of log returns on risky assets
sigma <- 0.175

### Figure 2 ###

rpiadj <- 0.5 # adjustment to convert cpi to rpi

#Assume Arithmetric mean return is 4.5# and the std dev of log returns is 16.6#
# Average arithmetic rate of return on risky asssets (equities)
mew <- 0.045 

# st deviation of log returns on risky assets
sigma <- 0.175

### Figure 3 and Figure 4 ###

rpiadj <- 0.5 # adjustment to convert cpi to rpi

# Initial assets 
#Update from 31 July 2021 file:///C:/Users/ew18103/Downloads/Monitoring%20Dashboard%2031%20Jul%202021.pdf
Cashflows[1,1]<- -80.6

#Assume Arithmetric mean return is 4.5# and the std dev of log returns is 16.6#
# Average arithmetic rate of return on risky asssets (equities)
mew <- 0.045 

# st deviation of log returns on risky assets
sigma <- 0.175

### Figure 6 ###

rpiadj <- 0.5 # adjustment to convert cpi to rpi

# Initial assets 
#Update from 31 July 2021 file:///C:/Users/ew18103/Downloads/Monitoring%20Dashboard%2031%20Jul%202021.pdf
Cashflows[1,1]<- -80.6

# Average arithmetic rate of return on risky asssets (equities)
mew <- 0.074

# st deviation of log returns on risky assets
sigma <- 0.205

### Figure 7 ###

rpiadj <- 0.5 # adjustment to convert cpi to rpi

# Initial assets 
#Update from 31 July 2021 file:///C:/Users/ew18103/Downloads/Monitoring%20Dashboard%2031%20Jul%202021.pdf
Cashflows[1,1]<- -80.6

# Average arithmetic rate of return on risky asssets (equities)
mew <- 0.0735

# st deviation of log returns on risky assets
sigma <- 0.1381

####################################################################
##  Base Case using Real Rates as of March 2020
####################################################################

TCashflows <- -(Cashflows+EXtraflows)/CPI
RealDiscountRates <- ((1+ (RealSpot+rpiadj)/100)^((0:(periods-1))))
RealForwardRate <- unlist(c(RealSpot[1,1]+rpiadj, (RealDiscountRates[2:periods,1]/RealDiscountRates[1:(periods-1),1]-1)*100))
RealForwardRateBase <- RealForwardRate

####################################################################
##  Now look at Case with Risky Investment (no mean reversion)
####################################################################

#As we are working with no mean reversion then
pp <- 0 # length of MA process
theta <- 0 # MA coefficient

#Returns on risky assets
saferate <- RealForwardRateBase

# mean of log returns to give average arithmetic return = mewrisky
lmew <- log((1+mew)^2/(sqrt(sigma^2+(1+mew)^2)))
lsigma  <- sqrt(log(1+sigma^2/(1+mew)^2))
lsigma_adj  <- lsigma/((1+pp*(theta^2))^0.5)

#share of portfolio in risky assets
alpha_range<-(seq(0.25,0.75, by=0.1))[-2]

set.seed(89)
FundsDistrib_all<-list()

for (k in 1:length(alpha_range)){
  
  #k<-5
  
  #share of portfolio in risky assets
  alpha <- alpha_range[k]
  
  #number of histories for simulation
  nsims <- 100000
  Paths<-matrix(0,periods,nsims)
  
  for(i in 1:nsims){
    errors <- rnorm(periods+pp)
    if (pp > 0){
      tmp <- c(cumsum(errors[1:pp]), sapply(1:periods, function(x) sum(errors[x:(pp+x)])))
      errors <- errors - theta * c(0, tmp[1:(periods+pp-1)])
      errors <- errors[(pp+1):(periods+pp)]
    }
    
    Paths[1,i]<- TCashflows[1,1]
    
    for (j in  2:periods){
      #j<-2
      Paths[j,i] <- (exp(lmew +errors[j]*lsigma_adj)*alpha+(1-alpha)*(1+saferate[j]/100))* Paths[j-1,i] + TCashflows[j,1]
    }
  }
  
  # estimate
  FundsSizeCuts <- c(0, 50, 100, 200, 400)
  FundsDistrib <- matrix(0, periods, length(FundsSizeCuts)+1)
  
  for (i in  1:periods){
    #i<-1
    foo <- which(Paths[i,]< 0)
    Paths[i:periods,foo] <- 0
    
    for (j in 1:length(FundsSizeCuts)){
      #j<-1
      FundsDistrib[i, j] <- sum(Paths[i,] <= FundsSizeCuts[j])
    }
    
    FundsDistrib[i, length(FundsSizeCuts)+1] <- nsims
    FundsDistrib[i,2:(length(FundsSizeCuts)+1)] <- diff(FundsDistrib[i,])
    
  }
  
  FundsDistrib_all[[k]]<-FundsDistrib/nsims
}


### Plotting Barplots for 75% equities ###

A<-as.matrix(FundsDistrib_all[[5]][c(1,11,21,31,41,61,81),], 6,7)
rownames(A)<-c("2020","2030","2040","2050","2060","2080","2100")
colnames(A)<-c("Funds exhausted", "0-50", "50-100","100-200","200-400",">400")

A[,seq(6,1,by=-1)] %>%
  melt(id.vars = rownames(A)) %>%
  ggplot(aes(x = as.factor(Var1), y = value*100, fill = Var2)) + geom_bar(stat = 'identity') + 
  labs(x="year",y="percentage",fill="funds")

### Plotting likelihood of funds exhausted ###
pal <- colorRampPalette(c("red", "blue"))
col_alpha<-pal(length(alpha_range))

plot(seq(2020,2102,by=1),FundsDistrib_all[[1]][,1], type="l", ylim=c(0,1), col=col_alpha[1], ylab="Funds exhausted", xlab="Year")
for (i in 2:length(alpha_range)){
  lines(seq(2020,2102,by=1), FundsDistrib_all[[i]][,1], type="l", col=col_alpha[i])
}
legend("topleft", legend=paste(alpha_range*100, "%", sep=""), col=col_alpha, bty = "n", ncol=2, lty=1)

#save(FundsDistrib_all, file="USSBriefs2021_Fig2.RData")
#save(FundsDistrib_all, file="USSBriefs2021_Fig3and4.RData")
#save(FundsDistrib_all, file="USSBriefs2021_Fig6.RData")
#save(FundsDistrib_all, file="USSBriefs2021_Fig7.RData")

####################################################################
##  Figure 5
####################################################################

### load data ###

#USS
USS_updated<-read_excel("USS_ForwardRates_from_gilt_yeild_CPI_basis.xlsx", sheet="USS_Data_nominal")

#Bank of England 
# Note that BoE only gives prediction 40 years in the future
BoE_CPI <- read_excel("NominalForwardRate_BoE.xlsx", sheet="Sheet2")

### Nominal Forward rate ###

# M&S 
NominalForwardRate<- (USS_updated["CPI"]*100)+RealForwardRate[-1]  

# USS
NominalForwardRate_USS<-USS_updated["Gilt yield"]*100

# Bank of England
NominalForwardRate_BoE<-as.data.frame(BoE_CPI)

### Figure ###

plot(NominalForwardRate_BoE[,1],NominalForwardRate_BoE[,2], type="l", ylim=c(-1,3), lty=3,
     ylab="forward nominal rate", xlab="years")
lines(NominalForwardRate_BoE[,1],NominalForwardRate_BoE[,3], type="l")
lines(NominalForwardRate[1:40,], type="l", col="blue") 
lines(NominalForwardRate_USS[1:40,], type="l", col="red")
abline(h=0)

legend(x = "topleft", legend = c("BoE March 2020", "BoE March 2021", "M&S", "USS"),  # Legend texts
       lty = c(3, 1, 1, 1),           # Line types
       col = c("black", "black", "blue", "red"), horiz = T,  bty = "n")
