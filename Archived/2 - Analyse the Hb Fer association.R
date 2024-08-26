#########################################################
# Analyse change in Hb as function of various parameters 
#########################################################
rm(list=ls())
while (!is.null(dev.list())) dev.off()
cat("\014")  

source("0 - Load_ferritin_data.R")
# This source code should load a dataset and two functions (select_males_only and 
# select_females_only) to either make a selection of either male or female donors.
# The data requires the two variables lFer and dRefHb which represent
# log10 Ferritin levels and the difference between baseline and current Hb level

source("1 - Support functions.R")
# various support functions required for the analyses

# Set simulation parameters
dobayes<-F         # perform a bayesian changepoint analysis if set to T
dobootstrap<-T     # perform a bootstrap of the max likelihood estimate for the changepoint if set to T
nrbssims<-4      # nr of bootstrapsamples
bsseed<-2          # seedvalue for the bootstrap
rollmean_nr<-1000  # nr of samples to use in the rolling mean
dotest<-F          # perform a quick testrun on 1000 samples (T) of full analyses (F)

# if bootstrapresults are obtained these are saved. Saved results can be read in if specified here
MaleBootstrapResultFile  <-"MaleBootstrap_1000samp_seed2__YYYY-MM-DD.RDS"
FemaleBootstrapResultFile<-"MaleBootstrap_1000samp_seed2__YYYY-MM-DD.RDS"

# lower and upper limits for the changepoint fit function to limit the search area
# these consist of the intercept, slope, changepoint lFer and standard deviation 
# of the error of the fitted line
ll=c(-2, 0.005,  -4, 0.3) # lower limit 
ul=c( 1,     2,   4, 0.9) # upper limit

# load various libraries
library("Hmisc")
library("zoo")
library("mcp") # for bayesian changepoint analysis
library("rjags")
library("pracma")


#################################
# Analyse male donors
#################################

### set male dataset for reference
refdata<-select_males_only()
refdata<-refdata[order(refdata$lFer),] # order data by lFer
nrow(refdata) # nr of donations
length(unique(refdata$KeyID)) # nr of donors
nrow(refdata)/length(unique(refdata$KeyID)) # nr of donations per donor
if(dotest) refdata<-refdata[sample(1:nrow(refdata), 1000,replace = F),]

# copy reference dataset for fitting models (and bootstrapping)
fitdata<-refdata


### plot lFer vs dRefHb
xm<-plotHbFer()


### bootstrap this result
if(dobootstrap){ 
  bsxm<-c()
  nr<-nrow(fitdata)
  set.seed(bsseed)
  for (i in 1:nrbssims){
    fitdata<-refdata[sample(1:nr, nr, replace = T), c("lFer","dRefHb")]
    bsr<-fminsearch(fitf, xm, lower=ll, upper=ul, method="Hooke-Jeeves")
    bsxm<-rbind(bsxm, c(bsr$xmin, bsr$convergence, bsr$info$iterations, SolutionOk(bsr$xmin)))
    # print(paste(i,":",paste(bsr$xmin, collapse = " "), bsr$convergence, bsr$info$iterations))
    print(paste(i,":",paste(bsxm[i,], collapse = " ")))
  }
  MaleBootstrapResultFile<-paste0("MaleBootstrap_",nrbssims,"samp_seed",bsseed,"_",Sys.Date(),".RDS")
  saveRDS(bsxm, file=MaleBootstrapResultFile)
  fitdata<-refdata
}

if(file.exists(MaleBootstrapResultFile)){
  
  # Read bootstrap results
  bsxm<-readRDS(MaleBootstrapResultFile)

  # Calculate and plot 95% confidence intervals for the bootstrap
  # 95% confidence intervals for the lines
  x<-seq(0,5, .01)
  bse<-c()
  for(i in 1:nrow(bsxm)) bse<-rbind(bse,yvalsfitted(x,bsxm[i,]))
  llimfit<-apply(bse, 2, function(x) quantile(x, 0.025))
  ulimfit<-apply(bse, 2, function(x) quantile(x, 0.975))
  CIcp<-quantile(bsxm[,3], c(.025,0.975))
  CIsm<-list(x=x, llimfit=llimfit, ulimfit=ulimfit, CIcp=CIcp)
  plotHbFer(CIsm)
  
  # plot distribution of the changepoint
  plot(density(bsxm[,3], bw=.003))
  quantile(bsxm[,3], c(.025,0.975))
  10^quantile(bsxm[,3], c(.025,0.975))
}


### perform a bayesian changepoint analyses
if(dobayes) { 
  ferritinmodel = list(
    Hb ~ 1+lFer,  # slope (int_1)
    ~ 0 + lFer    # joined slope (time_2) at cp_1
  )
  (malefit = mcp(ferritinmodel, data = fitdata))
  plot(malefit)
  summary(malefit)
  saveRDS(malefit, file="maleBayesfit.rds")

  ferritinmodel = list(
    Hb ~ 1+lFer,  # slope (int_1)
    ~ 0           # joined plateau cp_1
  )
  (malefit2 = mcp(ferritinmodel, data = fitdata))
  plot(malefit2)
  summary(malefit2)
  saveRDS(malefit2, file="maleBayesfit2.rds")
}


### analyse subgroups
# various parameters in the dataset may be analysed here
# the function plotCovariateDependency uses either a nr of cutpoints or vector with cutpoints
plotCovariateDependency("age",5) 
plotCovariateDependency("age",c(25,33,50)) 
plotCovariateDependency("Hb",5)
plotCovariateDependency("BMI",5)
plotCovariateDependency("numdon",5)
#plotCovariateDependency("numdon",c(3,4,5,6,10,15))


#################################
# Analyse female donors
#################################

### set female dataset for reference
refdata<-select_females_only()
refdata<-refdata[order(refdata$lFer),] # order data by lFer
nrow(refdata) # nr of donations
length(unique(refdata$KeyID)) # nr of donors
nrow(refdata)/length(unique(refdata$KeyID)) # nr of donations per donor
if(dotest) refdata<-refdata[sample(1:nrow(refdata), 1000,replace = F),]

# copy reference dataset for fitting models (and bootstrapping)
fitdata<-refdata


### plot lFer vs dRefHb
xf<-plotHbFer()


### now bootstrap this result
if(dobootstrap){ 
  bsxf<-c()
  nr<-nrow(fitdata)
  set.seed(bsseed)
  for (i in 1:nrbssims){
    fitdata<-refdata[sample(1:nr, nr, replace = T), c("lFer","dRefHb")]
    bsr<-fminsearch(fitf, xf, lower=ll, upper=ul, method="Hooke-Jeeves")
    bsxf<-rbind(bsxf, c(bsr$xmin, bsr$convergence, bsr$info$iterations, SolutionOk(bsr$xmin)))
    # print(paste(i,":",paste(bsr$xmin, collapse = " "), bsr$convergence, bsr$info$iterations))
    print(paste(i,":",paste(bsxf[i,], collapse = " ")))
  }
  FemaleBootstrapResultFile<-paste0("MaleBootstrap_",nrbssims,"samp_seed",bsseed,"_",Sys.Date(),".RDS")
  saveRDS(bsxf, file=FemaleBootstrapResultFile)
  fitdata<-refdata
}

if(file.exists(FemaleBootstrapResultFile)){

  # Read bootstrap results
  bsxf<-readRDS(FemaleBootstrapResultFile)
  
  # Calculate and plot 95% confidence intervals for the bootstrap
  # 95% confidence intervals for the lines
  x<-seq(0,5, .01)
  bse<-c()
  for(i in 1:nrow(bsxf)) bse<-rbind(bse,yvalsfitted(x,bsxf[i,]))
  llimfit<-apply(bse, 2, function(x) quantile(x, 0.025))
  ulimfit<-apply(bse, 2, function(x) quantile(x, 0.975))
  CIcp<-quantile(bsxf[,3], c(.025,0.975))
  CIsf<-list(x=x, llimfit=llimfit, ulimfit=ulimfit, CIcp=CIcp)
  plotHbFer(CIsf)
  
  # plot distribution of the changepoint
  plot(density(bsxf[,3], bw=.003))
  quantile(bsxf[,3], c(.025,0.975))
  10^quantile(bsxf[,3], c(.025,0.975))
} 


### perform a bayesian changepoint analyses
if(dobayes) { 
  ferritinmodel = list(
    Hb ~ 1+lFer,  # slope (int_1)
    ~ 0 + lFer    # joined slope (time_2) at cp_1
  )
  (femalefit = mcp(ferritinmodel, data = fitdata))
  plot(femalefit)
  summary(femalefit)
  saveRDS(femalefit, file="femaleBayesfit.rds")
  
  ferritinmodel = list(
    Hb ~ 1+lFer,  # slope (int_1)
    ~ 0           # joined plateau cp_1
  )
  (femalefit2 = mcp(ferritinmodel, data = fitdata))
  plot(femalefit2)
  summary(femalefit2)
  saveRDS(femalefit2, file="femaleBayesfit2.rds")
}


### analyse subgroups
# various parameters in the dataset may be analysed here
# the function plotCovariateDependency uses either a nr of cutpoints or vector with cutpoints
fitdata<-refdata
plotCovariateDependency("age",5) # uses either nr of cutpoints, or vector with cutpoints
plotCovariateDependency("age",c(25,33,50)) 
plotCovariateDependency("Hb",5)
plotCovariateDependency("BMI",5)
plotCovariateDependency("numdon",5)
plotCovariateDependency("numdon",c(3,4,5,6,10,15))
