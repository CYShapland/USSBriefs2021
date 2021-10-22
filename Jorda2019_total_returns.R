##############################################################################################################
# Code for calculating mean and sd of globally diversified portfolio from Jorda et al. 2019
# Authors: Neil M Davies, Jackie Grant, Chin Yang Shapland
# Last updated: 19/10/21
###############################################################################################################

### Data extraction ###
#Jordea's zip file from https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/GGDQGJ/RDNLLW&version=1.1
library(haven)

### REMOVE directory ###
dataset <- read_stata("RORE_QJE_replication/RORE_QJE_replication/data/rore_public_main.dta")
dataset_supp <- read_stata("RORE_QJE_replication/RORE_QJE_replication/data/rore_public_supplement.dta")

#Generate total real GDP for each country
real_gdp_all<-subset(dataset_supp, select=c("iso", "year", "pop","rgdpmad", "inflation"))
eq_tr<-subset(dataset, select=c("iso","year","eq_tr"))
data_all<-merge(real_gdp_all, eq_tr, by=c("iso", "year"))

## Generate total real GDP for each country
data_all["real_gdp"]<-data_all$pop*data_all$rgdpmad

#Calculating weighted total returns
years<-unique(data_all$year)
# estimate annual returns to get total returns
total_returns<-rep(0,length(years))

for(i in 1:length(years)){
  #i<-54
  data_year<-subset(data_all, year==years[i])
  
  #real returns from each country
  r_eq_tr = ((1 + data_year$eq_tr)/(1 + data_year$inflation))-1
  
  data_year$real_gdp<-ifelse(is.na(r_eq_tr),0,data_year$real_gdp)

  #real gdp in proportion to population size and real GDP per capita
  share_gdp=data_year$real_gdp/sum(data_year$real_gdp)
  
  #real returns weighted by each country's real gdp
  annual_returns<-share_gdp*r_eq_tr
  
  total_returns[i]<-sum(annual_returns, na.rm = T)

}

mean(total_returns)
sd(total_returns)
