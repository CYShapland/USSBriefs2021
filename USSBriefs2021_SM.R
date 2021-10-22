##############################################################################################################
# Code for Figure in Supplementary material of "The USS Trustee's risky strategy" 2021
# Authors: Neil M Davies, Jackie Grant, Chin Yang Shapland
# Last updated: 19/10/21
###############################################################################################################

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
