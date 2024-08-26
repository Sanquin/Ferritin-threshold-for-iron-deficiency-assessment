################################################
# functions to calculate and plot changepoints 
################################################

# function to plot the lFer vs dRefHb association with a fit of the changepoint
# and a rolling mean of the data 
# if confidence intervals are provided these are plotted as well
plotHbFer<-function(CIs) {
  set.seed(1)
  with(fitdata,plot(jitter(lFer,amount=.1),dRefHb, xlim=c(0.2,3), ylim=c(-2.5,2.5), 
                    xaxt="n", xlab="Ferritin [ng/mL]", ylab="Hb change from baseline value [mmol/L]"))
  xvals<-log10(15*2^(-8:8))
  xlabs<-(15*2^(-8:8))
  axis(1, at=xvals,labels=xlabs,las=1)
  abline(h=0,col=8)
  if (!missing(CIs)) { # Add information on confidence intervals
    lines(CIs$x,CIs$ulimfit, col=8, lty=3)
    lines(CIs$x,CIs$llimfit, col=8, lty=3)
    # 95% confidence intervals for the changepoint
    abline(v=CIs$CIcp, col=8, lty=3)
  }
  xm<-plotsol()
  print(paste("Changepoint:",10^xm[3]))
  with(fitdata[order(fitdata$lFer),], 
       lines(lFer, rollmean(dRefHb, rollmean_nr, fill = list(NA, NULL, NA)),  col = 3,  lwd = 2))
  abline(v=xm[3],col=7, lwd=2, lty=2)
  print(paste("SolutionOk:",SolutionOk(xm)))
  if (missing(CIs)) { # Add information on confidence intervals
    legend("topleft", c("Fitted association", "Changepoint level", paste0("Rolling mean (n=", rollmean_nr,")")), 
           lty=c(1,2,1), lwd=c(2,2,2), col=c(2,7,3))
  } else {
    legend("topleft", c("Fitted association", "Changepoint level", paste0("Rolling mean (n=", rollmean_nr,")"), "95% Confidence interval"), 
           lty=c(1,2,1,3), lwd=c(2,2,2,1), col=c(2,7,3,8))
  }
  return(xm)
}

# function to estimate and plot the changepoint on the fitdata dataset
plotsol<-function(col=2, lwd=2){
  # set initial values vor the changepoint using the linear regression results
  lfit<-lm(dRefHb~lFer, data=fitdata)
  x<-as.numeric(c(lfit$coefficients, -lfit$coefficients[1]/lfit$coefficients[2], sd(lfit$residuals)))
  # x<-as.numeric(c(lfit$coefficients, 1.5, sd(lfit$residuals)))
  
  # calculate the parameter estimates which maximize the likelihood function
  sol<-fminsearch(fitf, x, lower=ll, upper=ul, method="Hooke-Jeeves")

  # plot and print the results
  xs<-c(0, sol$xmin[3],5)
  lines(xs, yvalsfitted(xs, sol$xmin), col=col, lwd=lwd)
  print(paste(paste(sol$xmin, collapse = " "), sol$convergence, sol$info$iterations), SolutionOk(sol$xmin))
  return(sol$xmin)
}

# Rough check on the minimum found
SolutionOk<-function(x){
  o<-c()
  for (i in 1:4) {
    x2<-x
    x2[i]<-x[i]+1e-5
    o<-c(o,fitf(x2)-fitf(x))
  }
  # return(paste(paste(o, collapse=" "), ifelse(length(which(o<0))>0, "ERROR", "")))
  # if false then not a minimum
  return(ifelse(length(which(o<0))>0, F, T))
}

fitf<-function(x) {
  # function to calculate the likelihood of a function with a linear increase with intercept x[1]  
  # and slope x[2], a subsequent changepoint at x[3] after which the y-value remains constant
  # and random error with standard deviation x[4]
  # this function makes use of the fitdata
  bt<-fitdata$lFer>x[3]
  -sum(dnorm(fitdata$dRefHb[!bt]-(x[1]+x[2]*fitdata$lFer[!bt]), mean=0, sd=x[4], log=T))-
   sum(dnorm(fitdata$dRefHb[ bt]-(x[1]+x[2]*x[3]             ), mean=0, sd=x[4], log=T))
}

yvalsfitted<-function(x, sol){
  # function to calculate y values for a given set of x values and changepoint parameters (sol)
  ifelse(x>sol[3], sol[1]+sol[2]*sol[3], sol[1]+sol[2]*x)
}

# function to evaluate and plot the fit for various subgroups
# the subgroups parameter is defined by the covariate input,
# the cutpoints (cuts) can be either specified by an input vector
# or by an integer value indicating the number of cutpoints
plotCovariateDependency<-function(covariate, cuts) {
  set.seed(1)
  with(refdata,plot(jitter(lFer,amount=.1),dRefHb, xlim=c(0.2,3), ylim=c(-2.5,2.5), 
                    xaxt="n", xlab="Ferritin [ng/mL]", ylab="Hb change from baseline value [mmol/L]"))
  xvals<-log10(15*2^(-8:8))
  xlabs<-(15*2^(-8:8))
  axis(1, at=xvals,labels=xlabs,las=1)
  abline(h=0,col=8)
  
  if (length(cuts)==1) { # by ncuts quantiles
    # set quantiles
    eval(parse(text=paste0("quantiles <- quantile(refdata$",covariate,", prob = seq(0, 1, length = cuts+1))")))
    # add quantile info to the data
    eval(parse(text=paste0("refdata$",covariate,"cuts <- cut2(refdata$",covariate,", cuts = quantiles)")))
  } else { # by directly specifying cutpoints
    eval(parse(text=paste0("refdata$",covariate,"cuts <- cut2(refdata$",covariate,", cuts = cuts)")))
  }
  eval(parse(text=paste0("dist<-table(refdata$",covariate,"cuts)")))
  print(dist)
  ncuts<-length(dist)
  
  xa<-c()
  for (i in 1:length(dist))  {
    # fitdata<-refdata[refdata$agecuts==levels(refdata$agecuts)[i],]
    eval(parse(text=paste0("fitdata<<-refdata[refdata$",covariate,"cuts==levels(refdata$",covariate,"cuts)[i],]")))
    xa<-rbind(xa,plotsol(col=i+1))
    with(fitdata,lines(lFer, rollmean(dRefHb, rollmean_nr, fill = list(NA, NULL, NA)),  col = i+1,  lwd = 2))
  }
  paste0(levels(fitdata$agecuts), " n=" ,dist, "")
  # legend("topleft", levels(fitdata$agecuts), col=2:(ncuts+1),lwd=rep(2,ncuts), title="age groups")
  eval(parse(text=paste0("legend(\"topleft\", paste0(levels(fitdata$",covariate,"cuts), \" n=\" ,dist), col=2:(ncuts+1),lwd=rep(2,ncuts), title=\"",covariate," groups\")")))
  return(xa)
}
