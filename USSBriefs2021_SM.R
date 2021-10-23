##############################################################################################################
# Code for Figure in Supplementary material of "The USS Trustee's risky strategy" 2021
# Authors: Neil M Davies, Jackie Grant, Chin Yang Shapland
# Last updated: 19/10/21
###############################################################################################################

### load data ###

USS_CashFlows <- read_excel("USS_CashFlows.xlsx", sheet = "QuickCalcs")
Cashflows<-as.data.frame(USS_CashFlows["Expected cash flows (?bn) in relation to benefits accrued at 31 March 2020"])
CPI <- USS_CashFlows["CPI Index"]
EXtraflows <- USS_CashFlows["Expected cash flows (?bn) in relation to benefits projected to be accrued in 2020/21"]
RealSpot <- USS_CashFlows["Spot Real Yields"]
periods <- nrow(USS_CashFlows) #periods in model

#USS
USS_updated<-read_excel("USS_ForwardRates_from_gilt_yeild_CPI_basis.xlsx", sheet="USS_Data_nominal")

#Bank of England 
# Note that BoE only gives prediction 40 years in the future
BoE_CPI <- read_excel("NominalForwardRate_BoE.xlsx", sheet="Sheet2")

### Real Forward rate ###

# M&S 
rpiadj<-0.5

RealDiscountRates <- ((1+ (RealSpot+rpiadj)/100)^((0:(periods-1))))
RealForwardRate_MS <- unlist((RealDiscountRates[2:periods,1]/RealDiscountRates[1:(periods-1),1]-1)*100)

# USS
RealForwardRate_USS<- (USS_updated["Gilt yield"]-USS_updated["CPI"])*100

# Bank of England
NominalForwardRate_BoE<-as.data.frame(BoE_CPI)
colnames(NominalForwardRate_BoE)

years<-seq(1,nrow(NominalForwardRate_BoE), by=2)

NominalForwardRate_BoE2020_years<-sapply(1:length(years), function(x) mean(NominalForwardRate_BoE[years[x]:(years[x]+1),2]))
NominalForwardRate_BoE2021_years<-sapply(1:length(years), function(x) mean(NominalForwardRate_BoE[years[x]:(years[x]+1),3]))

RealForwardRate_BoE<-data.frame(Years=seq(2020,2059, by=1), 
                                RealForwardRate_2020=((NominalForwardRate_BoE2020_years/100)-USS_updated[1:40,"CPI"])*100,
                                RealForwardRate_2021=((NominalForwardRate_BoE2021_years/100)-0.025)*100)

### Figure ###

plot(RealForwardRate_BoE[,1], RealForwardRate_BoE[,2],  type="l", ylim=c(-2.5,0), lty=3,
     ylab="forward nominal rate", xlab="years")
lines(RealForwardRate_BoE[,1], RealForwardRate_BoE[,3], type="l")
lines(RealForwardRate_BoE[,1],RealForwardRate_MS[1:40], type="l", col="blue") 
lines(RealForwardRate_BoE[,1],RealForwardRate_USS[1:40,], type="l", col="red")
abline(h=0)

legend(x = "topleft", legend = c("BoE March 2020", "BoE March 2021", "M&S", "USS"),  # Legend texts
       lty = c(3, 1, 1, 1),           # Line types
       col = c("black", "black", "blue", "red"), horiz = T,  bty = "n")
