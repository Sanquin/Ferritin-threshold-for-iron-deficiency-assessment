#########################################################
# Load ferritin dataset
#########################################################
FileToUse<-"S:/Donaties/Donaties2012-2022_220917_with_BMI.RDS"
data<-readRDS(FileToUse)
colnames(data)
sapply(data,class)
data$Geslacht<-as.factor(data$Geslacht)
# data$cols<-as.factor(data$cols)
data$Donatie<-as.factor(data$Donatie)

#################################
# Select a subset of male donors
#################################
select_males_only<-function(){
  sel<-with(data, which(Geslacht =="M" & Donatie =="V"))
  length(sel) # 2418697
  sum(data$numdon[sel]==0) # 154168
  sel<-sel[data$numdon[sel]>0]
  length(sel) # 2264529
  sel<-sel[!is.na(data$nextHbind[sel]) & !is.na(data$Hb[data$nextHbind[sel]]) & !is.na(data$Hb[sel])]
  length(sel) # 2041394
  table(round(data$Hb[sel],1))
  sel<-sel[data$Hb[sel]>5 & data$Hb[sel]<15]
  length(sel) # 2041394
  table(round(abs(data$Hb[data$nextHbind[sel]]-data$Hb[sel]),1))
  sel<-sel[abs(data$Hb[data$nextHbind[sel]]-data$Hb[sel])<6]
  length(sel) # 2041368
  sel<-sel[data$nextVisHbDt[sel]<2*365] # to ensure dynamical change
  length(sel) # 2011167
  table(round(data$nextVisHbDt[sel]/10,0)*10)
  table(data$nextVisHbDt[sel][data$nextVisHbDt[sel]<55])
  sel<-sel[data$nextVisHbDt[sel]>50] # make sure there is a meaningful time passed
  length(sel) # 2010912
  sum(is.na(data$BMI[sel]))
  sel<-sel[!is.na(data$BMI[sel])] 
  length(sel) # 2009361
  
  # only donations with ferritin values and full history
  sel<-sel[!is.na(data$Ferritine[sel])]
  length(sel) # 137706
  sel<-sel[data$Hb[sel]>=8.4 & data$Hb[sel]<15]
  length(sel) # 137699
  #select donations only!
  sel<-sel[data$numdon[sel]>0]
  length(sel) # 137699
  
  sel<-sel[data$ND[sel]==T]
  length(sel) # 51607
  
  # select only donors with at least two donations
  twodons<-data$KeyID[sel][data$numdon[sel]>1]
  sel<-sel[data$KeyID[sel] %in% twodons]
  # select only donations over 3
  sel<-sel[data$numdon[sel]>2]
  length(sel) # 50390
  length(unique(data$KeyID[sel])) # 29469 donors
  
  # Create sub-dataset
  datt<-with(data,as.data.frame(cbind(KeyID[sel], nextVisHb[sel]-Hb[sel], DonatieTijd[nextHbind[sel]]-DonatieTijd[sel], DonAge[sel], Hb[sel], 
                                      BMI[sel], numdon[sel], ldt[sel], temp[nextHbind[sel]]-temp[sel], Hb[sel]-refHb[sel], Ferritine[sel], ldt[sel], ldtFer[sel])))
  colnames(datt)<-c("KeyID", "dHb", "dDontime", "age", "Hb", "BMI", "numdon", "lTime", "dTemp", "dRefHb" , "Ferritin", "ldt", "ldtFer")
  datt$lFer<-log10(datt$Ferritin)
  
  return(datt)

}




################################################
# Select a subset of female donors
################################################
select_females_only<-function(){
  
  sel<-with(data, which(Geslacht =="F" & Donatie =="V"))
  length(sel) # 2381684
  sum(data$numdon[sel]==0) # 273966
  sel<-sel[data$numdon[sel]>0]
  length(sel) # 2107718
  sel<-sel[!is.na(data$nextHbind[sel]) & !is.na(data$Hb[data$nextHbind[sel]]) & !is.na(data$Hb[sel])& !is.na(data$DonAge[sel])]
  length(sel) # 1776963
  table(round(data$Hb[sel],1))
  sel<-sel[data$Hb[sel]>5 & data$Hb[sel]<15]
  length(sel) # 1776942
  table(round(abs(data$Hb[data$nextHbind[sel]]-data$Hb[sel]),1))
  sel<-sel[abs(data$Hb[data$nextHbind[sel]]-data$Hb[sel])<6]
  length(sel) # 1776862
  sel<-sel[data$nextVisHbDt[sel]<2*365] # to ensure dynamical change
  length(sel) # 1724598
  table(round(data$nextVisHbDt[sel]/10,0)*10)
  table(data$nextVisHbDt[sel][data$nextVisHbDt[sel]<65])
  sel<-sel[data$nextVisHbDt[sel]>50] # make sure there is a meaningful time passed
  length(sel) # 1724588
  sum(is.na(data$BMI[sel]))
  sel<-sel[!is.na(data$BMI[sel])] 
  length(sel) # 1718965
  
  # only donations with ferritin values and full history
  sel<-sel[!is.na(data$Ferritine[sel])]
  length(sel) # 128693
  sel<-sel[data$Hb[sel]>=7.8 & data$Hb[sel]<15]
  length(sel) # 128692
  #select donations only!
  sel<-sel[data$numdon[sel]>0]
  length(sel) # 128692
  
  sel<-sel[data$ND[sel]==T]
  length(sel) # 62100
  
  # select only donors with at least two donations
  twodons<-data$KeyID[sel][data$numdon[sel]>1]
  sel<-sel[data$KeyID[sel] %in% twodons]
  # select only donations over 3
  sel<-sel[data$numdon[sel]>2]
  length(sel) # 49862
  length(unique(data$KeyID[sel])) # 35292 donors
  
  # Create sub-dataset
  datt<-with(data,as.data.frame(cbind(KeyID[sel], nextVisHb[sel]-Hb[sel], DonatieTijd[nextHbind[sel]]-DonatieTijd[sel], DonAge[sel], Hb[sel], 
                                      acat[sel], BMI[sel], numdon[sel], ldt[sel], temp[nextHbind[sel]]-temp[sel], Hb[sel]-refHb[sel], Ferritine[sel], ldt[sel], ldtFer[sel])))
  colnames(datt)<-c("KeyID", "dHb", "dDontime", "age", "Hb", "acat", "BMI", "numdon", "lTime", "dTemp", "dRefHb" , "Ferritin", "ldt", "ldtFer")
  datt$lFer<-log10(datt$Ferritin)
  
return(datt)

}
