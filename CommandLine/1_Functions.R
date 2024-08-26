# load bootstrap files #############

# set some stuff here to be sure
STD_CONSTANT <<- 0.6
constant_slope <<- T
constant_std <<- T

load_bootstrap_files <- function(x) {
  bootstrap_files <<- list.files(x, full.names = T, recursive = FALSE)
  print("Files found:")
  print(bootstrap_files)
  females_bootstrap <<- bootstrap_files[grepl("Female", bootstrap_files) & !grepl("combined", bootstrap_files)]
  males_bootstrap <<- bootstrap_files[grepl("/Male", bootstrap_files) & !grepl("combined", bootstrap_files)]
  if (menopausal_status) {
    premenopausal_bootstrap <<- bootstrap_files[grepl("Premenopausal", bootstrap_files) & !grepl("combined", bootstrap_files)]
    postmenopausal_bootstrap <<- bootstrap_files[grepl("Postmenopausal", bootstrap_files) & !grepl("combined", bootstrap_files)]
  }
  print("")
  print("Files that will not be merged into the combined file (because they are combined already):")
  print(bootstrap_files[grepl("combined", bootstrap_files)])
}

load_combined_bootstrap <- function(x) {
  combined_bootstrap <<- list.files(x, full.names = T, recursive = FALSE)
  combined_bootstrap <<- combined_bootstrap[grepl("combined", combined_bootstrap)]
  MaleBootstrapResultFile <<- combined_bootstrap[grepl("Male", combined_bootstrap)]
  FemaleBootstrapResultFile <<- combined_bootstrap[grepl("Female", combined_bootstrap)]
  if (menopausal_status) {
    PremenopausalBootstrapResultFile <<- combined_bootstrap[grepl("Premenopausal", combined_bootstrap)]
    PostmenopausalBootstrapResultFile <<- combined_bootstrap[grepl("Postmenopausal", combined_bootstrap)]
  }
}

# Select a subset of male donors ##################
select_males_only <- function() {
  sel <- with(data, which(Geslacht == "M" & Donatiesoortcode == "Volbloed"))
  length(sel)
  sel <- sel[data$AfgenomenVolume[sel] > 400]
  length(sel)
  sel <- sel[!is.na(data$Hb[sel])]
  length(sel)
  table(round(data$Hb[sel], 1))
  # sel<-sel[data$Hb[sel]>5 & data$Hb[sel]<15]
  length(sel)
  
  # only donations with ferritin values and full history
  sel <- sel[!is.na(data$Ferritine[sel])]
  length(sel)
  # sel<-sel[data$Hb[sel]>=8.4 & data$Hb[sel]<15]
  # length(sel)
  
  with(data[sel, ], table(numdon))
  # Identify donors with first ferritin measurement at first intake
  selnewfer <- with(data, which(!is.na(Ferritine) & Donatiesoortcode == "Nieuwe donorkeuring"))
  
  sel <- sel[!(data$KeyID[sel] %in% data$KeyID[selnewfer])]
  length(sel)
  
  # Create sub-dataset
  datt <- with(data, as.data.frame(cbind(KeyID[sel], Leeftijd[sel], Hb[sel], numdon[sel], Hb[sel] - RefHb[sel], Ferritine[sel])))
  colnames(datt) <- c("KeyID", "age", "Hb", "numdon", "dRefHb", "Ferritin")
  datt$lFer <- log10(datt$Ferritin)
  
  return(datt)
}

# Select a subset of female donors #############################
select_females_only <- function() {
  sel <- with(data, which(Geslacht == "F" & Donatiesoortcode == "Volbloed"))
  length(sel)
  sel <- sel[data$AfgenomenVolume[sel] > 400]
  length(sel)
  sel <- sel[!is.na(data$Hb[sel])]
  length(sel)
  table(round(data$Hb[sel], 1))
  # sel<-sel[data$Hb[sel]>5 & data$Hb[sel]<15]
  # length(sel)
  
  # only donations with ferritin values and full history
  sel <- sel[!is.na(data$Ferritine[sel])]
  length(sel)
  # sel<-sel[data$Hb[sel]>=7.8 & data$Hb[sel]<15]
  # length(sel)
  
  with(data[sel, ], table(numdon))
  # Identify donors with first ferritin measurement at first intake
  selnewfer <- with(data, which(!is.na(Ferritine) & Donatiesoortcode == "Nieuwe donorkeuring"))
  
  sel <- sel[!(data$KeyID[sel] %in% data$KeyID[selnewfer])]
  length(sel)
  
  # Create sub-dataset
  datt <- with(data, as.data.frame(cbind(KeyID[sel], Leeftijd[sel], Hb[sel], numdon[sel], Hb[sel] - RefHb[sel], Ferritine[sel])))
  colnames(datt) <- c("KeyID", "age", "Hb", "numdon", "dRefHb", "Ferritin")
  datt$lFer <- log10(datt$Ferritin)
  
  return(datt)
}

# Select a subset of premenopausal female donors #############################
select_premenopausal_only <- function() {
  sel <- with(data, which(Geslacht == "F" & Donatiesoortcode == "Volbloed" & premenopausal == 1))
  length(sel)
  sel <- sel[data$AfgenomenVolume[sel] > 400]
  length(sel)
  sel <- sel[!is.na(data$Hb[sel])]
  length(sel)
  table(round(data$Hb[sel], 1))
  # sel<-sel[data$Hb[sel]>5 & data$Hb[sel]<15]
  # length(sel)
  
  # only donations with ferritin values and full history
  sel <- sel[!is.na(data$Ferritine[sel])]
  length(sel)
  # sel<-sel[data$Hb[sel]>=7.8 & data$Hb[sel]<15]
  # length(sel)
  
  with(data[sel, ], table(numdon))
  # Identify donors with first ferritin measurement at first intake
  selnewfer <- with(data, which(!is.na(Ferritine) & Donatiesoortcode == "Nieuwe donorkeuring"))
  
  sel <- sel[!(data$KeyID[sel] %in% data$KeyID[selnewfer])]
  length(sel)
  
  # Create sub-dataset
  datt <- with(data, as.data.frame(cbind(KeyID[sel], Leeftijd[sel], Hb[sel], numdon[sel], Hb[sel] - RefHb[sel], Ferritine[sel])))
  colnames(datt) <- c("KeyID", "age", "Hb", "numdon", "dRefHb", "Ferritin")
  datt$lFer <- log10(datt$Ferritin)
  
  return(datt)
}

# Select a subset of premenopausal female donors #############################
select_postmenopausal_only <- function() {
  sel <- with(data, which(Geslacht == "F" & Donatiesoortcode == "Volbloed" & premenopausal == 0))
  length(sel)
  sel <- sel[data$AfgenomenVolume[sel] > 400]
  length(sel)
  sel <- sel[!is.na(data$Hb[sel])]
  length(sel)
  table(round(data$Hb[sel], 1))
  # sel<-sel[data$Hb[sel]>5 & data$Hb[sel]<15]
  # length(sel)
  
  # only donations with ferritin values and full history
  sel <- sel[!is.na(data$Ferritine[sel])]
  length(sel)
  # sel<-sel[data$Hb[sel]>=7.8 & data$Hb[sel]<15]
  # length(sel)
  
  with(data[sel, ], table(numdon))
  # Identify donors with first ferritin measurement at first intake
  selnewfer <- with(data, which(!is.na(Ferritine) & Donatiesoortcode == "Nieuwe donorkeuring"))
  
  sel <- sel[!(data$KeyID[sel] %in% data$KeyID[selnewfer])]
  length(sel)
  
  # Create sub-dataset
  datt <- with(data, as.data.frame(cbind(KeyID[sel], Leeftijd[sel], Hb[sel], numdon[sel], Hb[sel] - RefHb[sel], Ferritine[sel])))
  colnames(datt) <- c("KeyID", "age", "Hb", "numdon", "dRefHb", "Ferritin")
  datt$lFer <- log10(datt$Ferritin)
  
  return(datt)
}
# constant ------------
set_constant_slope_std <- function(constant_val_slope = TRUE, constant_val_std = TRUE) {
  # Convenience function to set ul/ll everytime constant is different
  # Also set start values for fit
  if (constant_val_slope) {
    # offset, slope1, logcp
    ll <<- c(-1, 0.01, 1) # lower limit (recommended: c(-2, 0.005,  -4))
    ul <<- c(1, 2, 2) # upper limit (recommended: c( 1,     2,   4))
    p0 <<- c(0, 0.8, 1.3)
  } else {
    # lower and upper limits for the changepoint fit
    # these consist of the intercept, slope, changepoint, slope of the second segment, standard deviation of the error
    ll <<- c(-1, 0.01, 1, -1) # lower limit (recommended: c(-2, 0.005,  1, -1))
    ul <<- c(1, 2, 2, 1) # upper limit (recommended: c(2,     2,   2, 1))
    p0 <<- c(0, 0.8, 1.3, 0)
  }
  if (!constant_val_std) {
    # std/sigma is fitted
    STD_CONSTANT <<- 0.6 # Ehhh set here
    ll <<- c(ll, 0.1)
    ul <<- c(ul, 3)
    p0 <<- c(p0, STD_CONSTANT)
  }
  constant_slope <<- constant_val_slope
  constant_std <<- constant_val_std
}


# Plot hb Fer ###################
draw_key_cust <- function(data, params, size) {
  if (data$colour == "#1D6996") {
    data$linewidth <- 0.5 
    data$linetype = "twodash"
    data$alpha = 0.75
    draw_key_smooth(data, params, size)
  } else {
    data$linewidth <- 1
    draw_key_smooth(data, params, size)
  }
}

plotHbFer<-function(CIs) {
  set.seed(1)
  if(exists("changepointplot")){rm(changepointplot)}
  plotcolours <- c("#5F4690","#1D6996","#38A6A5","#0F8554","#73AF48","#EDAD08","#E17C05","#CC503E","#94346E","#6F4070","#994E95","#666666")
  #plotcolours <- c("#88CCEE","#CC6677","#DDCC77","#117733","#332288","#AA4499","#44AA99","#999933","#882255","#661100","#6699CC","#888888")
  #plotcolours <- c("#7F3C8D","#11A579","#3969AC","#F2B701","#E73F74","#80BA5A","#E68310","#008695","#CF1C90","#f97b72","#4b4b8f","#A5AA99")
  if(grepl("Male", title)){plotname <- "male_changepoint"} else if(grepl("Female", title)){plotname <- "female_changepoint"}
  else if (grepl("Premenopausal", title)){plotname <- "premenopausal_changepoint"}
  else if(grepl("Postmenopausal", title)){plotname <- "postmenopausal_changepoint" }
  #if constant save different file
  if (constant_slope) {
    plotname <- paste0(plotname, "_constant_slope")
  }
  if (constant_std) {
    plotname <- paste0(plotname, "_constant_std")
  } 
  plotname <- paste0(plotname, ".png")
  png(filename=paste0(file.path(main_dir, export_folder,currentDate,FolderDataset),"/", plotname), width=6, height=4, units="in", res=100)
  fitdata<-fitdata[order(fitdata$lFer),]
  fitdata$RollingMean <- rollmean(fitdata$dRefHb, k = rollmean_nr, fill = list(NA, NULL, NA))
  changepointplot <<- ggplot(fitdata, aes(x = lFer, y = dRefHb)) +
    #geom_hex(bins=hexbin_size)+  # Use 'gray' fill color +
    geom_point(aes(x = jitter(lFer, amount = 0.1), y = jitter(dRefHb, amount = 0.1)), alpha = geompoint_transparency, color = "black", shape =1, size = 2)+
    #geom_point(aes(x = lFer, y = dRefHb), alpha = geompoint_transparency, color = "black", shape =1, size =1.6)+
    geom_line(aes(y = RollingMean, color = "Rolling Mean"), linewidth = 1.1, key_glyph="cust") + # Rolling mean line
    xlim(0.2, 3) +
    ylim(-2.5, 2.5) +
    labs(
      title = title,
      x = "Ferritin [ng/mL]",
      y = "Hb change from baseline value [mmol/L]"
    ) +
    scale_x_continuous(
      breaks = log10(15 * 2^(-8:8)),
      labels = (15 * 2^(-8:8)),
      limits = c(0.2, 3),
      expand = c(0, 0)
    ) +
    theme_classic() +
    theme(legend.text=element_text(size=8))+
    geom_hline(yintercept = 0, color = "gray", size =0.5)
  
  # scale_fill_gradient("Observations", low = "grey98", high = "black") #+ theme(legend.position = "left")
  xm <- plotsol()
  print(xm)
  print(paste("Changepoint:",10^xm[3]))
  changepointplot <<- changepointplot + geom_vline(aes(xintercept = xm[3], color = "Changepoint"), linewidth=0.65, key_glyph = "cust")  
  fitdata$lFerjit <- jitter(log10(fitdata$Ferritin), amount = 0.01)
  fitdata$rollmean <- with(fitdata[order(fitdata$lFerjit),], rollmean(dRefHb, rollmean_nr, fill = list(NA, NULL, NA)))
  rollmean_data <<- fitdata %>% select(lFerjit, rollmean)
  if (!missing(CIs)) { # Add information on confidence intervals
    changepointplot <<- changepointplot + geom_line(data = CIs, aes(x = x, y = ulimfit), col = "#38A6A5", linetype = "twodash", linewidth = 0.5, alpha = 0.75) +
      geom_line(data = CIs, aes(x = x, y = llimfit, color = "Confidence interval"), linetype = "twodash", key_glyph = "cust",linewidth = 0.5, alpha = 0.75) 
    if(constant_slope){changepointplot <<- changepointplot + geom_vline(xintercept = CIcp_con[1], col = "#38A6A5", linetype = "twodash",linewidth = 0.5, alpha = 0.75)+
      geom_vline(xintercept = CIcp_con[2], col = "#38A6A5", linetype="twodash",linewidth = 0.5, alpha = 0.75)}else{changepointplot <<- changepointplot + geom_vline(xintercept = CIcp_sloped[1], col = "#38A6A5", linetype = "twodash",linewidth = 0.5, alpha = 0.75)+
        geom_vline(xintercept = CIcp_sloped[2], col = "#38A6A5", linetype="twodash",linewidth = 0.5, alpha = 0.75)}
  }
  if (!missing(CIs)) { # Add information on confidence intervals
    changepointplot <<- changepointplot + scale_colour_manual(breaks = c("Rolling Mean","Fitted association", "Changepoint",  "Confidence interval"),values =c('Rolling Mean'=plotcolours[5], "Fitted association" = plotcolours[8], "Changepoint" = plotcolours[6], "Confidence interval" ="#38A6A5"), labels = c(paste0("Rolling mean\n(window = ", rollmean_nr, ")"), "Fitted association", paste0("Changepoint level: ", round(10^xm[3],1)), "95% Confidence interval"))+
      labs(color = " ")
    
  } else {
    changepointplot <<- changepointplot +scale_colour_manual(breaks = c("Rolling Mean", "Fitted association", "Changepoint"),values =c('Rolling Mean'=plotcolours[5], "Fitted association" = plotcolours[8], "Changepoint" = plotcolours[6]), labels = c(paste0("Rolling mean\n(window = ", rollmean_nr, ")"), "Fitted association",paste0("Changepoint level: ", round(10^xm[3],1))))+
      labs(color = " ")
  }
  #changepointplot <<- ggMarginal(changepointplot, type = "histogram")
  changepointplot <<- changepointplot + geom_line(data = changepointline, aes(x = xs, y = ys, color = "Fitted association"), alpha = 0.9, key_glyph="cust", linewidth = 0.70)
  print(changepointplot)
  dev.off()
  print("...done plotting")
  return(xm)
}

find_xmin_sol <- function(x, lFer, DHb, check_sol = F) {
  # print(".....")
  # print(x)
  # print(".....")
  if (constant_std) {
    std <- sd(DHb)
    fnc <- function(x) fitf(x, lFer, DHb, std, constant_slope)
    # sol <- optim(par = x, fn = function(x) fitf(x, lFer, DHb, STD_CONSTANT, constant_slope), method = "L-BFGS-B", lower = ll, upper = ul)
  } else {
    fnc <- function(x) fitf(x[1:length(x) - 1], lFer, DHb, x[length(x)], constant_slope)
    # sol <- optim(par = x, fn = function(x) fitf(x[1:length(x) - 1], lFer, DHb, x[length(x)], constant_slope), method = "L-BFGS-B", lower = ll, upper = ul)
  }
  sol <- optim(par = x, fn = fnc, method = "L-BFGS-B", lower = ll, upper = ul)
  xmin <- sol$par
  # print(sol)
  if (check_sol) {
    if (!SolutionOk(fnc, xmin)) {
      print(xmin)
      print("Doing fminsearch")
      sol <- fminsearch(fnc, x, lower = ll, upper = ul, method = "Hooke-Jeeves")
      xmin <- sol$xmin
      print(paste("Solution oK? fminsearch:", SolutionOk(fnc, xmin)))
      print(xmin)
    }
  }
  return(sol)
}

get_xmin <- function(sol) {
  if ("xmin" %in% names(sol)) {
    return(sol$xmin)
  } else {
    return(sol$par)
  }
}

get_fmin <- function(sol) {
  if ("fmin" %in% names(sol)) {
    return(sol$fmin)
  } else {
    return(sol$value)
  }
}



# function to estimate and plot the changepoint on the fitdata dataset ##################
plotsol <- function(col = 2, lwd = 2) {
  # set initial values for the changepoint using the linear regression results
  
  # lfit<-lm(dRefHb~lFer, data=refdataM)
  # x<-as.numeric(c(lfit$coefficients, -lfit$coefficients[1]/lfit$coefficients[2], sd(lfit$residuals),0))
  # x<-as.numeric(c(lfit$coefficients, 1.5, sd(lfit$residuals)))
  # start parameters
  #This is OK
  x <- p0
  if (all(x > ll) & all(x < ul)) {
    lFer <- fitdata$lFer
    DHb <- fitdata$dRefHb
    sol <- find_xmin_sol(x, lFer, DHb)
    xmin <- get_xmin(sol)
    fmin <- get_fmin(sol)
    
    # plot and print the results
    # xmin = sol$xmin
    xs <- c(0.2, xmin[3], 3)
    
    #TODO Should run set_constant... function here, but what is clopse
    ys <- yvalsfitted(xs, xmin, constant_slope)
    changepointline <<- data.frame(xs,ys)
    #changepointplot <<- changepointplot + geom_line(data = changepointline, aes(x = xs, y = ys, color = "Fitted association"), alpha = 1, key_glyph="cust", linewidth = 0.5)
    # print(paste(paste(xmin, collapse = " "), SolutionOk(xmin, lFer, DHb)))
    sol_data <<- as.data.frame(cbind(xs, yvalsfitted(xs, xmin, constant_slope)))
    names(sol_data) <<- c("x", "y")
    
    return(xmin)
  } else {
    print("Can't conduct changepoint analysis because (some) parameters are outside limits you defined")
    limits <- (x > ul) + (x < ll)
    limits <- case_when(limits == 1 ~ "outside", limits == 0 ~ "inside")
    print(limits)
  }
  # calculate the parameter estimates which maximize the likelihood function
}

# Rough check on the minimum found #######################
SolutionOk <- function(fnc, x) {
  o <- c()
  np <- length(x)
  for (i in 1:np) {
    x2 <- x
    x2[i] <- x[i] + 1e-5
    o <- c(o, fnc(x2) - fnc(x))
  }
  # return(paste(paste(o, collapse=" "), ifelse(length(which(o<0))>0, "ERROR", "")))
  # if false then not a minimum
  return(ifelse(length(which(o < 0)) > 0, F, T))
}


# Function to calculate likelihood ##################
fitf <- function(sol, x, y, std = STD_CONSTANT, cslope = T) {
  # function to calculate the likelihood of a function with a linear increase with intercept x[1]
  # and slope x[2], a subsequent changepoint at x[3] after which the y-value remains constant_slope
  # and random error with standard deviation x[4] and x[5] as the slope of the second segment
  # this function makes use of the fitdata
  # x <- fitdata$lFer
  # y <- fitdata$dRefHb
  # bt <- x > sol[3]
  yfit <- yvalsfitted(x, sol, cslope)
  # if (constant_std) {
  #   std <- STD_CONSTANT # default 0.6
  # } else {
  #   std <- sol[length(sol)] # last value in vector
  # }
  # left <- -sum(dnorm(y[!bt] - yfit[!bt], mean = 0, sd = std, log = T))
  # right <- -sum(dnorm(y[bt] - yfit[bt], mean = 0, sd = std, log = T))
  total <- -sum(dnorm(y - yfit, mean = 0, sd = std, log = T))
  return(total)
}

# Calculate y values for given x and changepoint ###############
yvalsfitted <- function(x, sol, cslope = T) {
  # cs = constant slope
  # function to calculate y values for a given set of x values and changepoint parameters (sol)
  if (cslope) {
    return(ifelse(x < sol[3], sol[1] + sol[2] * (x - sol[3]), sol[1]))
  } else {
    return(ifelse(x < sol[3], sol[1] + sol[2] * (x - sol[3]), sol[1] + sol[4] * (x - sol[3])))
  }
}

# function to evaluate and plot the fit for various subgroups #################
# the subgroups parameter is defined by the covariate input,
# the cutpoints (cuts) can be either specified by an input vector
# or by an integer value indicating the number of cutpoints
plotCovariateDependency <- function(covariate, cuts, sex) {
  set.seed(1)
  if (sex == "M") {
    title <- "Male donors"
  } else if (sex == "F") {
    title <- "Female donors"
  } else {
    title <- "All donors"
  }
  xlab <- "Ferritin (ng/mL)"
  ylab <- "Hb change from reference value (mmol/L)"
  xaxt <- "n"
  eval(parse(text = paste0("plot<-with(refdata", sex, ",plot(jitter(lFer,amount=.1),dRefHb, xlim=c(0.2,3), ylim=c(-2.5,2.5),
                    xaxt=xaxt, xlab= xlab, ylab= ylab, main=title))")))
  xvals <- log10(15 * 2^(-8:8))
  xlabs <- (15 * 2^(-8:8))
  axis(1, at = xvals, labels = xlabs, las = 1)
  abline(h = 0, col = 8)
  
  if (length(cuts) == 1) { # by ncuts quantiles
    # set quantiles
    # eval(parse(text=paste0("quantiles <- quantile(refdata",sex,"$",covariate,", prob = seq(0, 1, length = cuts+1))")))
    # add quantile info to the data
    eval(parse(text = paste0("refdata", sex, "$", covariate, "cuts <- cut2(refdata", sex, "$", covariate, ", g = cuts)")))
  } else { # by directly specifying cutpoints
    eval(parse(text = paste0("refdata", sex, "$", covariate, "cuts <- cut2(refdata", sex, "$", covariate, ", cuts = cuts)")))
  }
  eval(parse(text = paste0("dist<-table(refdata", sex, "$", covariate, "cuts)")))
  print(dist)
  ncuts <- length(dist)
  
  xa <- c()
  for (i in 1:length(dist)) {
    # fitdata<-refdata[refdata$agecuts==levels(refdata$agecuts)[i],]
    eval(parse(text = paste0("fitdata<<-refdata", sex, "[refdata", sex, "$", covariate, "cuts==levels(refdata", sex, "$", covariate, "cuts)[i],]")))
    
    xa <- rbind(xa, plotsol(col = i + 1))
    with(fitdata, lines(lFer, rollmean(dRefHb, rollmean_nr, fill = list(NA, NULL, NA)), col = i + 1, lwd = 2))
  }
  paste0(levels(fitdata$agecuts), " n=", dist, "")
  # legend("topleft", levels(fitdata$agecuts), col=2:(ncuts+1),lwd=rep(2,ncuts), title="age groups")
  eval(parse(text = paste0("legend(\"topright\",cex=0.5, paste0(levels(fitdata$", covariate, "cuts), \" n=\" ,dist), col=2:(ncuts+1),lwd=rep(2,ncuts), title=\"", covariate, " groups\")")))
  return(xa)
}

# function to export the results #################
exportData <- function(data) {
  set.seed(1)
  if (exists("export_data") && is.data.frame(get("export_data"))) {
    export_data2 <- data[sample(nrow(data), proportion * nrow(data)), ] %>%
      mutate(dRefHbjit = jitter(dRefHb, amount = 0.01), lFerjit = jitter(log10(Ferritin), amount = 0.01)) %>%
      select(dRefHbjit, lFerjit)
    if (grepl("Male", title)) {
      export_data2$sex <- "M"
    } else if (grepl("Female", title)) {
      export_data2$sex <- "F"
    } else if (grepl("Premenopausal", title)) {
      export_data2$sex <- "preF"
    } else if (grepl("Postmenopausal", title)) {
      export_data2$sex <- "postF"
    }
    export_data <<- rbind(export_data, export_data2)
    # rm(export_data2)
  } else {
    export_data <<- data[sample(nrow(data), proportion * nrow(data)), ] %>%
      mutate(dRefHbjit = jitter(dRefHb, amount = 0.01), lFerjit = jitter(log10(Ferritin), amount = 0.01)) %>%
      select(dRefHbjit, lFerjit)
    if (grepl("Male", title)) {
      export_data$sex <<- "M"
    } else if (grepl("Female", title)) {
      export_data$sex <<- "F"
    } else if (grepl("Premenopausal", title)) {
      export_data$sex <<- "preF"
    } else if (grepl("Postmenopausal", title)) {
      export_data$sex <<- "postF"
    }
  }
  rownames(export_data) <- NULL
  saveRDS(export_data, file = paste0(file.path(main_dir, export_folder, currentDate, FolderDataset), "/export_data.rds"))
}

exportRollmean <- function(data) {
  if (exists("rollmean_export") && is.data.frame(get("rollmean_export"))) {
    rollmean_export2 <- data
    if (grepl("Male", title)) {
      rollmean_export2$sex <- "M"
    } else if (grepl("Female", title)) {
      rollmean_export2$sex <- "F"
    } else if (grepl("Premenopausal", title)) {
      rollmean_export2$sex <- "preF"
    } else if (grepl("Postmenopausal", title)) {
      rollmean_export2$sex <- "postF"
    }
    rollmean_export <<- rbind(rollmean_export, rollmean_export2)
  } else {
    rollmean_export <<- data
    if (grepl("Male", title)) {
      rollmean_export$sex <<- "M"
    } else if (grepl("Female", title)) {
      rollmean_export$sex <<- "F"
    } else if (grepl("Premenopausal", title)) {
      rollmean_export$sex <<- "preF"
    } else if (grepl("Postmenopausal", title)) {
      rollmean_export$sex <<- "postF"
    }
  }
  rownames(rollmean_export) <- NULL
  saveRDS(rollmean_export, file = paste0(file.path(main_dir, export_folder, currentDate, FolderDataset), "/rollmean_export.rds"))
}

exportSolution <- function(data) {
  if (exists("sol_export") && is.data.frame(get("sol_export"))) {
    sol_export2 <- data
    if (grepl("Male", title)) {
      sol_export2$sex <- "M"
      if (constant_slope) {
        sol_export2$constant_slope <- T
      } else {
        sol_export2$constant_slope <- F
      }
      if (constant_std) {
        sol_export2$constant_std <- T
      } else {
        sol_export2$constant_std <- F
      }
    } else if (grepl("Female", title)) {
      sol_export2$sex <- "F"
      if (constant_slope) {
        sol_export2$constant_slope <- T
      } else {
        sol_export2$constant_slope <- F
      }
      if (constant_std) {
        sol_export2$constant_std <- T
      } else {
        sol_export2$constant_std <- F
      }
    } else if (grepl("Premenopausal", title)) {
      sol_export2$sex <- "preF"
      if (constant_slope) {
        sol_export2$constant_slope <- T
      } else {
        sol_export2$constant_slope <- F
      }
      if (constant_std) {
        sol_export2$constant_std <- T
      } else {
        sol_export2$constant_std <- F
      }
    } else if (grepl("Postmenopausal", title)) {
      sol_export2$sex <- "postF"
      if (constant_slope) {
        sol_export2$constant_slope <- T
      } else {
        sol_export2$constant_slope <- F
      }
      if (constant_std) {
        sol_export2$constant_std <- T
      } else {
        sol_export2$constant_std <- F
      }
    }
    sol_export <<- rbind(sol_export, sol_export2)
  } else {
    sol_export <<- data
    if (grepl("Male", title)) {
      sol_export$sex <<- "M"
      if (constant_slope) {
        sol_export$constant_slope <<- T
      } else {
        sol_export$constant_slope <<- F
      }
      if (constant_std) {
        sol_export$constant_std <<- T
      } else {
        sol_export$constant_std <<- F
      }
    } else if (grepl("Female", title)) {
      sol_export$sex <<- "F"
      if (constant_slope) {
        sol_export$constant_slope <<- T
      } else {
        sol_export$constant_slope <<- F
      }
      if (constant_std) {
        sol_export$constant_std <<- T
      } else {
        sol_export$constant_std <<- F
      }
    } else if (grepl("Premenopausal", title)) {
      sol_export$sex <<- "preF"
      if (constant_slope) {
        sol_export$constant_slope <<- T
      } else {
        sol_export$constant_slope <<- F
      }
      if (constant_std) {
        sol_export$constant_std <<- T
      } else {
        sol_export$constant_std <<- F
      }
    } else if (grepl("Postmenopausal", title)) {
      sol_export$sex <<- "postF"
      if (constant_slope) {
        sol_export$constant_slope <<- T
      } else {
        sol_export$constant_slope <<- F
      }
      if (constant_std) {
        sol_export$constant_std <<- T
      } else {
        sol_export$constant_std <<- F
      }
    }
  }
  rownames(sol_export) <- NULL
  saveRDS(sol_export, file = paste0(file.path(main_dir, export_folder, currentDate, FolderDataset), "/sol_export.rds"))
}

exportConfInt <- function(CIs) {
  if (exists("llimexport") && is.data.frame(get("llimexport"))) {
    llimexport2 <- CIs$llimfit
    llimexport2$constant_slope <- constant_slope
    if (grepl("male", plotsex)) {
      llimexport2$sex <- "M"
    } else if (grepl("female", plotsex)) {
      llimexport2$sex <- "F"
    } else if (grepl("premenopausal", plotsex)) {
      llimexport2$sex <- "PreF"
    } else if (grepl("postmenopausal", plotsex)) {
      llimexport2$sex <- "PostF"
    }
    llimexport <- rbind(llimexport, llimexport2)
  } else {
    llimexport <<- CIs$llimfit
    llimexport$constant_slope <- constant_slope
    if (grepl("male", plotsex)) {
      llimexport$sex <- "M"
    } else if (grepl("female", plotsex)) {
      llimexport$sex <- "F"
    } else if (grepl("premenopausal", plotsex)) {
      llimexport$sex <- "PreF"
    } else if (grepl("postmenopausal", plotsex)) {
      llimexport$sex <- "PostF"
    }
  }
  if (exists("ulimexport") && is.data.frame(get("ulimexport"))) {
    ulimexport2 <- CIs$ulimfit
    ulimexport2$constant_slope <- constant_slope
    if (grepl("male", plotsex)) {
      ulimexport2$sex <- "M"
    } else if (grepl("female", plotsex)) {
      ulimexport2$sex <- "F"
    } else if (grepl("premenopausal", plotsex)) {
      ulimexport2$sex <- "PreF"
    } else if (grepl("postmenopausal", plotsex)) {
      ulimexport2$sex <- "PostF"
    }
    ulimexport <- rbind(ulimexport, ulimexport2)
  } else {
    ulimexport <<- CIs$ulimfit
    ulimexport$constant_slope <- constant_slope
    if (grepl("male", plotsex)) {
      ulimexport$sex <- "M"
    } else if (grepl("female", plotsex)) {
      ulimexport$sex <- "F"
    } else if (grepl("premenopausal", plotsex)) {
      ulimexport$sex <- "PreF"
    } else if (grepl("postmenopausal", plotsex)) {
      ulimexport$sex <- "PostF"
    }
  }
  if (exists("CIcp_export") && is.data.frame(get("CIcp_export"))) {
    CIcp_export2 <- CIs$CIcp
    CIcp_export2$constant_slope <- constant_slope
    if (grepl("male", plotsex)) {
      CIcp_export2$sex <- "M"
    } else if (grepl("female", plotsex)) {
      CIcp_export2$sex <- "F"
    } else if (grepl("premenopausal", plotsex)) {
      CIcp_export2$sex <- "PreF"
    } else if (grepl("postmenopausal", plotsex)) {
      CIcp_export2$sex <- "PostF"
    }
    CIcp_export <- rbind(CIcp_export, CIcp_export2)
  } else {
    CIcp_export <<- CIs$CIcp
    CIcp_export$constant_slope <- constant_slope
    if (grepl("male", plotsex)) {
      CIcp_export$sex <- "M"
    } else if (grepl("female", plotsex)) {
      CIcp_export$sex <- "F"
    } else if (grepl("premenopausal", plotsex)) {
      CIcp_export$sex <- "PreF"
    } else if (grepl("postmenopausal", plotsex)) {
      CIcp_export$sex <- "PostF"
    }
  }
  rownames(ulimexport) <- NULL
  saveRDS(ulimexport, file = paste0(file.path(main_dir, export_folder, currentDate, FolderDataset), "/ulimexport.rds"))
  rownames(llimexport) <- NULL
  saveRDS(llimexport, file = paste0(file.path(main_dir, export_folder, currentDate, FolderDataset), "/llimexport.rds"))
  rownames(CIcp_export) <- NULL
  saveRDS(CIcp_export, file = paste0(file.path(main_dir, export_folder, currentDate, FolderDataset), "/CIcp_export.rds"))
}

# bootstrap--------------
compute_bss <- function(data, p0_constant, p0_slope, nbootstraps = 10) {
  # compute bootstrap fits and store parameters and likelihood
  # for constant_slope right part and non constant_slope (with slope) left part
  # Use optim instead of fminsearch to gain some (considerable) speed
  # ~ 4 times faster
  # the global variable constant_slope is set each time and this is pretty ugly imo
  # MartP
  # I do not store the convergence every time, because are we really checking?
  # Do store the check of the solution, but sometimes the optim (which uses)
  # L-BFGS-B does not find the exact minimum, but it is close enough for these bootstraps
  nr <- nrow(data)
  bs <- matrix(nrow = nbootstraps, ncol = 9) # hardcode 13 ehhh TODO
  colnames(bs) <- c("offset constant", "first slope constant", "changepoint constant", "likelihood constant", "offset sloped", "first slope sloped", "changepoint sloped", "second slope sloped", "likelihood sloped")
  lFer <- data$lFer
  DHb <- data$dRefHb
  for (i in 1:nbootstraps) {
    i_sample <- sample(1:nr, nr, replace = T)
    # fitdata<<-data[sample(1:nr, nr, replace = T), c("lFer","dRefHb")]
    # Only do std is constant (gain some speed)
    set_constant_slope_std(T, T)
    lFer_sample <- lFer[i_sample]
    DHb_sample <- DHb[i_sample]
    sol_constant_slope <- find_xmin_sol(p0_constant, lFer_sample,
                                        DHb_sample,
                                        check_sol = F
    )
    xmin_constant_slope <- get_xmin(sol_constant_slope)
    fmin_constant_slope <- get_fmin(sol_constant_slope)
    # First 3 values is constant_slope fit
    bs[i, 1:3] <- xmin_constant_slope
    bs[i, 4] <- fmin_constant_slope
    
    set_constant_slope_std(F, T)
    sol <- find_xmin_sol(p0_slope, lFer_sample, DHb_sample,
                         check_sol = F
    )
    xmin <- get_xmin(sol)
    fmin <- get_fmin(sol)
    
    # 6-9 is non constant_slope fit
    bs[i, 5:8] <- xmin
    bs[i, 9] <- fmin
    if (printbootstrap) {
      print(paste(i, ":", paste(bs[i, ], collapse = " ")))
    }
  }
  return(bs)
}

plot_bootstraps <- function(data, bs, sol_constant, sol_sloped) {
  fitdata <<- data
  if (grepl("Male", title)) {
    plotsex <<- "male"
  } else if (grepl("Female", title)) {
    plotsex <<- "female"
  } else if (grepl("Premenopausal", title)) {
    plotsex <<- "premenopausal"
  } else if (grepl("Postmenopausal", title)) {
    plotsex <<- "postmenopausal"
  }
  x <- seq(0, 5, .01)
  bse_sloped <- c()
  set_constant_slope_std(F, T) # always need to set this for yvalsfitted
  for (i in 1:nrow(bs)) {
    bse_sloped <- rbind(bse_sloped, yvalsfitted(x, bs[i, c("offset sloped", "first slope sloped", "changepoint sloped", "second slope sloped")], constant_slope))
  }
  llimfit_sloped <- apply(bse_sloped, 2, function(x) quantile(x, 0.025))
  ulimfit_sloped <- apply(bse_sloped, 2, function(x) quantile(x, 0.975))
  CIcp_sloped <<- as.numeric(c(quantile(bs[, "changepoint sloped"], c(.025, 0.975))))
  CIsm_sloped <<- data.frame(x = x, llimfit = llimfit_sloped, ulimfit = ulimfit_sloped)
  plotHbFer(CIsm_sloped)
  if (export_files) {
    exportConfInt(CIsm_sloped)
  }
  
  set_constant_slope_std(T, T) # always need to set this for yvalsfitted
  fitdata <<- data
  bse_con <- c()
  for (i in 1:nrow(bs)) {
    bse_con <- rbind(bse_con, yvalsfitted(x, bs[i, c("offset constant", "first slope constant", "changepoint constant")], constant_slope))
  }
  llimfit_con <- apply(bse_con, 2, function(x) quantile(x, 0.025))
  ulimfit_con <- apply(bse_con, 2, function(x) quantile(x, 0.975))
  CIcp_con <<- as.numeric(c(quantile(bs[, "changepoint constant"], c(.025, 0.975))))
  CIsm_con <<- data.frame(x = x, llimfit = llimfit_con, ulimfit = ulimfit_con)
  plotHbFer(CIsm_con)
  if (export_files) {
    exportConfInt(CIsm_con)
  }
  
  png(filename = paste0(file.path(main_dir, export_folder, currentDate, FolderDataset), "/", plotsex, "_density.png"), width = 6, height = 6, units = "in", res = 100)
  plot(density(bs[, "changepoint constant"], bw = .003))
  lines(density(bs[, "changepoint sloped"], bw = .003), col = "red", lty = "dashed")
  # legend(1, 95, legend=c('constant_slope', 'non-constant_slope'), col=c('black', 'red'), lty=1:2, cex=0.8) #does not work
  dev.off()
  
  # png(filename=paste0(file.path(main_dir, export_folder,currentDate,FolderDataset),"/",plotsex,"_likelihood.png"), width=6, height=6, units="in", res=100)
  # plot(density(bs[, 4]))
  # lines(density(bs[, 11]), col='red', lty='dashed')
  # dev.off()
  
  png(filename = paste0(file.path(main_dir, export_folder, currentDate, FolderDataset), "/", plotsex, "_likelihood_ratio.png"), width = 6, height = 6, units = "in", res = 100)
  plot(density(bs[, "likelihood constant"] - bs[, "likelihood sloped"]))
  dev.off()
  
  png(filename = paste0(file.path(main_dir, export_folder, currentDate, FolderDataset), "/", plotsex, "_second_slope.png"), width = 6, height = 6, units = "in", res = 100)
  plot(density(bs[, "second slope sloped"]))
  dev.off()
  
  results_matrix <- matrix(nrow = 5, ncol = 6)
  colnames(results_matrix) <- c("measure", "sex", "constant second slope", "estimate", "ll", "ul")
  results_matrix[, 1] <- c("Changepoint", "Slope first segment", "Changepoint", "Slope first segment", "Slope second segment")
  results_matrix[, 2] <- plotsex
  results_matrix[1:2, 3] <- T
  results_matrix[3:5, 3] <- F
  
  print("constant_second_slope")
  print(paste0("CI of changepoint: [", 10^quantile(bs[, "changepoint constant"], c(.025, 0.975))[1], ",", 10^quantile(bs[, "changepoint constant"], c(.025, 0.975))[2], "]"))
  results_matrix[1, 4] <- 10^sol_constant[3]
  results_matrix[1, 5:6] <- 10^quantile(bs[, "changepoint constant"], c(.025, 0.975))
  print(paste0("CI of slope first segment: [", quantile(bs[, "first slope constant"], c(.025, 0.975))[1], ",", quantile(bs[, "first slope constant"], c(.025, 0.975))[2], "]"))
  results_matrix[2, 4] <- sol_constant[2]
  results_matrix[2, 5:6] <- quantile(bs[, "first slope constant"], c(.025, 0.975))
  print("non-constant_scond_slope")
  print(paste0("CI of changepoint: [", 10^quantile(bs[, "changepoint sloped"], c(.025, 0.975))[1], ",", 10^quantile(bs[, "changepoint sloped"], c(.025, 0.975))[2], "]"))
  results_matrix[3, 4] <- 10^sol_sloped[3]
  results_matrix[3, 5:6] <- 10^quantile(bs[, "changepoint sloped"], c(.025, 0.975))
  print(paste0("CI of slope first segment: [", quantile(bs[, "first slope sloped"], c(.025, 0.975))[1], ",", quantile(bs[, "first slope sloped"], c(.025, 0.975))[2], "]"))
  results_matrix[4, 4] <- sol_sloped[2]
  results_matrix[4, 5:6] <- quantile(bs[, "first slope sloped"], c(.025, 0.975))
  print(paste0("CI of slope second segment: [", quantile(bs[, "second slope sloped"], c(.025, 0.975))[1], ",", quantile(bs[, "second slope sloped"], c(.025, 0.975))[2], "]"))
  results_matrix[5, 4] <- sol_sloped[4]
  results_matrix[5, 5:6] <- quantile(bs[, "second slope sloped"], c(.025, 0.975))
  
  saveRDS(results_matrix, file = paste0(file.path(main_dir, export_folder, currentDate, FolderDataset), "/", plotsex, "_results_matrix.rds"))
}