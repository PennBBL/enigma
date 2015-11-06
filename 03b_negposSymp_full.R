###################################################################################################################################
##################### this script will produce information on data availability, basic stats (such as mean,range,etc) ###########
##################### and distribution of your negative symptom score variable and other demographics/covariates ########################
##################################################################################################################################
## for questions, contact Esther Walton: esther.walton@uniklinikum-dresden.de


################ import data #######################

cat("importing cortical thickness data",'\n')
#comma-delimited with header, missing = NA:
Cort <- read.csv("CorticalMeasuresENIGMA_ThickAvg.csv", sep=",", header=TRUE); #Read in the phenotypes file

#keep CT variables of interest:
ROIs <- c("L_caudalanteriorcingulate_thickavg","R_caudalanteriorcingulate_thickavg", 
"L_caudalmiddlefrontal_thickavg","R_caudalmiddlefrontal_thickavg","L_lateralorbitofrontal_thickavg","R_lateralorbitofrontal_thickavg","L_medialorbitofrontal_thickavg","R_medialorbitofrontal_thickavg","L_parsopercularis_thickavg","R_parsopercularis_thickavg","L_parsorbitalis_thickavg","R_parsorbitalis_thickavg","L_parstriangularis_thickavg","R_parstriangularis_thickavg","L_rostralanteriorcingulate_thickavg","R_rostralanteriorcingulate_thickavg","L_rostralmiddlefrontal_thickavg","R_rostralmiddlefrontal_thickavg","L_superiorfrontal_thickavg","R_superiorfrontal_thickavg","L_frontalpole_thickavg","R_frontalpole_thickavg")
posROIs <- c("L_superiortemporal_thickavg","R_superiortemporal_thickavg")
allROIs <- c(ROIs,posROIs)
ROIsSubjID=append("SubjID",allROIs)


#prune for variables, which are present
cat("checking, which ROIs of interest in cortical thickness data","\n")
ROIsInCT <- ROIsSubjID %in% names(Cort)

ROIsSubjID=subset(ROIsSubjID, ROIsInCT)


Cort <- Cort[ROIsSubjID]
cat("keeping following variables:",ROIsSubjID,"\n")


# Check for duplicated SubjIDs that may cause issues with merging data sets.
if(anyDuplicated(Cort[,c("SubjID")]) != 0) { stop('You have duplicate SubjIDs in your CorticalMeasuresENIGMA_ThickAvg.csv file.\nMake sure 
there are no repeat SubjIDs.') }

cat("importing covariate data",'\n')
#comma-delimited with header, missing = NA:
Covs <- read.csv("Covariates.csv", sep=",", header=TRUE); #Read in the covariates file

#Covs <- read.csv("toDG-JH_newFS53allimaging_onlySCZ_cov_NA_154s_20141103.csv", sep=",", header=TRUE); #Read in the covariates file


# Check for duplicated SubjIDs that may cause issues with merging data sets.
if(anyDuplicated(Covs[,c("SubjID")]) != 0) { stop('You have duplicate SubjIDs in your Covariates.csv file.\nMake sure there are no repeat 
SubjIDs.') }

n.covs <- ncol(Covs) - 1; #Total number of covariates, -1 removes the SubjectID column
n.sites <- ncol(Covs)-which(colnames(Covs) == "Site1")+1; #Find the number of site variables
cat("number of covariates:",n.covs,'\n')
cat("number of sites:",n.sites,'\n')

#


#combine the files into one dataframe
merged_ordered = merge(Covs, Cort, by="SubjID");

#Check that the number of rows after merging is the same
if(nrow(Cort) != nrow(merged_ordered)){
  cat('WARNING: CorticalMeasuresENIGMA_ThickAvg.csv and Covariates.csv have non-matching SubjIDs.','\n')
  cat('Please make sure the number of subjects in your merged data set are as expected.','\n')
  cat('The number of SubjIDs in CorticalMeasuresENIGMA_ThickAvg.csv is: ',nrow(Cort),'\n')
  cat('The number of SubjIDs in the merged_ordered data set is: ',nrow(merged_ordered),'\n')
}

############################ convert PANSS_neg to SANS, if needed ################

cat('check, whether PANSS_NEG, SANS_TOTAL or SANS_GLOBAL negative symptom scores','\n')


if ('SANS_GLOBAL' %in% colnames(merged_ordered==TRUE)) {
  cat('keep SANS_GLOBAL variable','\n')
} else if ('PANSS_NEG' %in% colnames(merged_ordered==TRUE)) {
  SANS_GLOBAL = -2.0671 + (0.665 * merged_ordered$PANSS_NEG)
  merged_ordered$PANSS_NEG <- SANS_GLOBAL
  names(merged_ordered)[names(merged_ordered) == 'PANSS_NEG'] <- 'SANS_GLOBAL'
  cat('converted PANSS_NEG to SANS_GLOBAL','\n')
} else if ('SANS_TOTAL' %in% colnames(merged_ordered==TRUE)) {
  SANS_GLOBAL = 1.0863 + (0.2943 * merged_ordered$SANS_TOTAL)
  merged_ordered$SANS_TOTAL <- SANS_GLOBAL
  names(merged_ordered)[names(merged_ordered) == 'SANS_TOTAL'] <- 'SANS_GLOBAL'
  cat('converted SANS_TOTAL to SANS_GLOBAL','\n')
} else {
  stop('Cannot find PANSS_NEG, SANS_TOTAL or SANS_GLOBAL column','\n')
}
############################ convert PANSS_pos to SAPS, if needed ################

cat('check, whether PANSS_POS, SAPS_TOTAL or SAPS_GLOBAL positive symptom scores','\n')


if ('SAPS_GLOBAL' %in% colnames(merged_ordered==TRUE)) {
  cat('keep SAPS_GLOBAL variable','\n')
} else if ('PANSS_POS' %in% colnames(merged_ordered==TRUE)) {
  SAPS_GLOBAL = -3.222 + (0.567 * merged_ordered$PANSS_POS)
  merged_ordered$PANSS_POS <- SAPS_GLOBAL
  names(merged_ordered)[names(merged_ordered) == 'PANSS_POS'] <- 'SAPS_GLOBAL'
  cat('converted PANSS_POS to SAPS_GLOBAL','\n')
} else if ('SAPS_TOTAL' %in% colnames(merged_ordered==TRUE)) {
  SAPS_GLOBAL = 2.3526 + (0.1932 * merged_ordered$SAPS_TOTAL)
  merged_ordered$SAPS_TOTAL <- SAPS_GLOBAL
  names(merged_ordered)[names(merged_ordered) == 'SAPS_TOTAL'] <- 'SAPS_GLOBAL'
  cat('converted SAPS_TOTAL to SAPS_GLOBAL','\n')
} else {
  stop('Cannot find PANSS_POS, SAPS_TOTAL or SAPS_GLOBAL column','\n')
}

############################### check negSymp distribution ##############################

cat('checking SANS_GLOBAL distribution','\n')
shapiro_orig=shapiro.test(merged_ordered$SANS_GLOBAL)
png(filename="histogram_of_SANS_GLOBAL.png")
layout(matrix(c(1,2), nrow=2))
qqplot=qqnorm(merged_ordered$SANS_GLOBAL)
qqline(merged_ordered$SANS_GLOBAL,distribution = qnorm)

hist=hist(merged_ordered$SANS_GLOBAL)
#curve(dnorm(x, mean=mean(na.omit(merged_ordered$SANS_GLOBAL)), sd=sqrt(var(na.omit(merged_ordered$SANS_GLOBAL)))),add=TRUE) 
text(15, 15, paste("p-value=",shapiro_orig[2]))

dev.off()

############################### check posSymp distribution ##############################

cat('checking SAPS_GLOBAL distribution','\n')
shapiro_orig=shapiro.test(merged_ordered$SAPS_GLOBAL)
png(filename="histogram_of_SAPS_GLOBAL.png")
layout(matrix(c(1,2), nrow=2))
qqplot=qqnorm(merged_ordered$SAPS_GLOBAL)
qqline(merged_ordered$SAPS_GLOBAL,distribution = qnorm)

hist=hist(merged_ordered$SAPS_GLOBAL)
#curve(dnorm(x, mean=mean(na.omit(merged_ordered$SANS_GLOBAL)), sd=sqrt(var(na.omit(merged_ordered$SANS_GLOBAL)))),add=TRUE) 
text(15, 15, paste("p-value=",shapiro_orig[2]))

dev.off()

############################### check asymmetry ##############################

cat('checking asymmetry','\n')
parstri_LmR=sum(merged_ordered$L_parstriangularis_thickavg-merged_ordered$R_parstriangularis_thickavg,na.rm=TRUE)
parsop_LmR=sum(merged_ordered$L_parsopercularis_thickavg-merged_ordered$R_parsopercularis_thickavg,na.rm=TRUE)
png(filename="asymmetry.png")
layout(matrix(c(1,2), nrow=2))
hist_parstri=hist(merged_ordered$L_parstriangularis_thickavg-merged_ordered$R_parstriangularis_thickavg)
text(0.2, 20, paste("left minus right=",parstri_LmR))
hist_parsop=hist(merged_ordered$L_parsopercularis_thickavg-merged_ordered$R_parsopercularis_thickavg)
text(0.2, 20, paste("left minus right=",parsop_LmR))

dev.off()

# ############################ get basic stats ################################
# 
# Get n/means/sd/min/max for each of the structures

cat('get n/means/sd/min/max of cortical thickness structures','\n')
raw.means.CT=colMeans(merged_ordered[(ncol(Covs)+1):ncol(merged_ordered)], na.rm=T)
sd.raw.CT=rep(NA,(ncol(merged_ordered)-ncol(Covs)))
n.raw.CT=rep(NA,(ncol(merged_ordered)-ncol(Covs)))
min.raw.CT=rep(NA,(ncol(merged_ordered)-ncol(Covs)))
max.raw.CT=rep(NA,(ncol(merged_ordered)-ncol(Covs)))
for(z in (ncol(Covs)+1):ncol(merged_ordered)){
  sd.raw.CT[z-ncol(Covs)]=sd(merged_ordered[,z], na.rm=T)
  n.raw.CT[z-ncol(Covs)]=length(merged_ordered[which(!is.na(merged_ordered[,z])),z])
  min.raw.CT[z-ncol(Covs)]=min(merged_ordered[,z], na.rm=T)
  max.raw.CT[z-ncol(Covs)]=max(merged_ordered[,z], na.rm=T)
}

#Save raw values
cat('save n/mean/min/max/sd CT values','\n')
raw_merged.CT=rbind(raw.means.CT,sd.raw.CT,n.raw.CT,min.raw.CT,max.raw.CT)
save(raw_merged.CT, file="RawMergedCT.Rdata")

#Get n/mean/sd/min/max of demographics

cat('get n/means/sd/min/max of demographic variables','\n')
raw.means.dmg=colMeans(merged_ordered[2:ncol(Covs)], na.rm=T)
sd.raw.dmg=rep(NA,(ncol(Covs)-1))
n.raw.dmg=rep(NA,(ncol(Covs)-1))
min.raw.dmg=rep(NA,(ncol(Covs)-1))
max.raw.dmg=rep(NA,(ncol(Covs)-1))
for(z in (2:(ncol(Covs)))){
  sd.raw.dmg[z-1]=sd(merged_ordered[,z], na.rm=T)
  n.raw.dmg[z-1]=length(merged_ordered[which(!is.na(merged_ordered[,z])),z])
  min.raw.dmg[z-1]=min(merged_ordered[,z], na.rm=T)
  max.raw.dmg[z-1]=max(merged_ordered[,z], na.rm=T)
}

#bind and remove frequency values
cat('bind and remove frequency variables','\n')
raw_merged.dmg=rbind(raw.means.dmg,sd.raw.dmg,n.raw.dmg,min.raw.dmg,max.raw.dmg)

#remove frequency variables such as Sex, White, ETHNIC, RACE,CUR_PSY_DIS_DIAG,Site1+
site_cov=paste(colnames(merged_ordered)[(n.covs-n.sites+2):(n.covs+1)])
catVar=c("Sex", "ETHNICITY","CUR_PSY_DIS_DIAG","HAND","MEDICATION",site_cov)

raw_merged_num.dmg=raw_merged.dmg[,!(colnames(raw_merged.dmg) %in% catVar)]
cat('removed frequency variables:',catVar,'\n')

#get stats on frequency variables
cat('get list of counts of frequency variables','\n')
raw_merged_freq.dmg <- as.list(setNames(replicate(length(catVar),numeric(0), simplify = F), catVar[1:length(catVar)]))
 
for (i in catVar){
  raw_merged_freq.dmg[[i]]=table(merged_ordered[[i]])
}

#Save demographics values
cat('save n/mean/min/max/sd demographics values','\n')

save(raw_merged_num.dmg,raw_merged_freq.dmg, file="RawMergedDMG.Rdata")


cat('sucessfully saved descriptives (n/mean/min/max/sd/counts) of demographics variables and SANS_GLOBAL distribution checks','\n') 


############################################################################
######################### checking for correlations #######################

cat('checking for correlations','\n')

library(Hmisc)

merged_ordered_matrix=data.matrix(merged_ordered)
correlations=rcorr(merged_ordered_matrix)

save(correlations, file="correlations.Rdata")
cat('saved correlations','\n')




############################################################################
################################ neg symp: run regression ###############################

cat('negative symptoms: running simple regressions','\n')

#CT=SANS_GLOBAL+Age+Sex

if ("Sex" %in% names(merged_ordered) == TRUE & "Age" %in% names(merged_ordered) == TRUE){
  for (ROI in ROIsSubjID[c(-1,-(length(ROIsSubjID)-1):-length(ROIsSubjID))]){
    #print(ROI)
    form <- as.formula(paste(ROI, "~SANS_GLOBAL+Age+as.factor(Sex)"))
    out=lm(formula = form, data = merged_ordered)
    filename <- paste('lm_SANS_sex_age',ROI,sep='_') 
    assign(filename, out)
  }
  }else {
  cat('no sex or age variable')
}


if ("HAND" %in% names(merged_ordered) == TRUE){
cat('adding HAND to regressions with age and sex','\n')
for (ROI in ROIsSubjID[c(-1,-(length(ROIsSubjID)-1):-length(ROIsSubjID))]){
  #print(ROI)
  form <- as.formula(paste(ROI, "~SANS_GLOBAL+Age+as.factor(Sex)+as.factor(HAND)"))
  out=lm(formula = form, data = merged_ordered)
  filename <- paste('lm_SANS_sex_age_hand',ROI,sep='_') 
  assign(filename, out)
  }
  }else {
  cat('no HAND variable')
}


if ("LENGTH_OF_ILLNESS" %in% names(merged_ordered) == TRUE){
cat('adding LENGTH_OF_ILLNESS to regressions with age and sex','\n')
for (ROI in ROIsSubjID[c(-1,-(length(ROIsSubjID)-1):-length(ROIsSubjID))]){
  #print(ROI)
  form <- as.formula(paste(ROI, "~SANS_GLOBAL+Age+as.factor(Sex)+LENGTH_OF_ILLNESS"))
  out=lm(formula = form, data = merged_ordered)
  filename <- paste('lm_SANS_sex_age_LOI',ROI,sep='_') 
  assign(filename, out)
}
}else {
  cat('no LENGTH_OF_ILLNESS variable')
}

if ("PANSS_TOTAL" %in% names(merged_ordered) == TRUE){
cat('adding PANSS_TOTAL (illness severity) to regressions with age and sex','\n')
for (ROI in ROIsSubjID[c(-1,-(length(ROIsSubjID)-1):-length(ROIsSubjID))]){
  #print(ROI)
  form <- as.formula(paste(ROI, "~SANS_GLOBAL+Age+as.factor(Sex)+PANSS_TOTAL"))
  out=lm(formula = form, data = merged_ordered)
  filename <- paste('lm_SANS_sex_age_IllSev',ROI,sep='_') 
  assign(filename, out)
}
}else {
  cat('no PANSS_TOTAL variable')
}

if ("MEDICATION" %in% names(merged_ordered) == TRUE){
  cat('adding MEDICATION to regressions with age and sex','\n')
  for (ROI in ROIsSubjID[c(-1,-(length(ROIsSubjID)-1):-length(ROIsSubjID))]){
    #print(ROI)
    form <- as.formula(paste(ROI, "~SANS_GLOBAL+Age+as.factor(Sex)+as.factor(MEDICATION)"))
    out=lm(formula = form, data = merged_ordered)
    filename <- paste('lm_SANS_sex_age_MED',ROI,sep='_') 
    assign(filename, out)
  }
}else {
  cat('no MEDICATION variable')
}

cat('saving simple regression output','\n')
save(list=ls(pattern="lm_SANS_*"),file="lm_SANS_simple.RData")


############################################################################
################################ neg symp: run regression, with site ###############################

cat('negative symptoms: repeating regression models, this time including site','\n')

#CT=SANS_GLOBAL+Age+Sex

site_regr=paste(colnames(merged_ordered)[(n.covs-n.sites+2):(n.covs+1)],collapse="+")

if ("Sex" %in% names(merged_ordered) == TRUE & "Age" %in% names(merged_ordered) == TRUE & n.sites != 0){
  for (ROI in ROIsSubjID[c(-1,-(length(ROIsSubjID)-1):-length(ROIsSubjID))]){
    #print(ROI)
    form <- as.formula(paste(ROI, "~SANS_GLOBAL+Age+as.factor(Sex)+",site_regr))
    out=lm(formula = form, data = merged_ordered)
    filename <- paste('lm_SANS_site_sex_age',ROI,sep='_') 
    assign(filename, out)
  }
}else {
  cat('no sex, age or site variable')
}


if ("HAND" %in% names(merged_ordered) == TRUE & n.sites != 0){
  cat('adding HAND to regressions with age, sex and site','\n')
  for (ROI in ROIsSubjID[c(-1,-(length(ROIsSubjID)-1):-length(ROIsSubjID))]){
    #print(ROI)
    form <- as.formula(paste(ROI, "~SANS_GLOBAL+Age+as.factor(Sex)+as.factor(HAND)+",site_regr))
    out=lm(formula = form, data = merged_ordered)
    filename <- paste('lm_SANS_site_sex_age_hand',ROI,sep='_') 
    assign(filename, out)
  }
}else {
  cat('no HAND or site variable')
}


if ("LENGTH_OF_ILLNESS" %in% names(merged_ordered) == TRUE & n.sites != 0){
  cat('adding LENGTH_OF_ILLNESS to regressions with age, sex and site','\n')
  for (ROI in ROIsSubjID[c(-1,-(length(ROIsSubjID)-1):-length(ROIsSubjID))]){
    #print(ROI)
    form <- as.formula(paste(ROI, "~SANS_GLOBAL+Age+as.factor(Sex)+LENGTH_OF_ILLNESS+",site_regr))
    out=lm(formula = form, data = merged_ordered)
    filename <- paste('lm_SANS_site_sex_age_LOI',ROI,sep='_') 
    assign(filename, out)
  }
}else {
  cat('no LENGTH_OF_ILLNESS or site variable')
}

if ("PANSS_TOTAL" %in% names(merged_ordered) == TRUE & n.sites != 0){
  cat('adding PANSS_TOTAL (illness severity) to regressions with age, sex and site','\n')
  for (ROI in ROIsSubjID[c(-1,-(length(ROIsSubjID)-1):-length(ROIsSubjID))]){
    #print(ROI)
    form <- as.formula(paste(ROI, "~SANS_GLOBAL+Age+as.factor(Sex)+PANSS_TOTAL+",site_regr))
    out=lm(formula = form, data = merged_ordered)
    filename <- paste('lm_SANS_site_sex_age_IllSev',ROI,sep='_') 
    assign(filename, out)
  }
}else {
  cat('no PANSS_TOTAL or site variable')
}

if ("MEDICATION" %in% names(merged_ordered) == TRUE & n.sites != 0){
  cat('adding MEDICATION to regressions with age, sex and site','\n')
  for (ROI in ROIsSubjID[c(-1,-(length(ROIsSubjID)-1):-length(ROIsSubjID))]){
    #print(ROI)
    form <- as.formula(paste(ROI, "~SANS_GLOBAL+Age+as.factor(Sex)+as.factor(MEDICATION)+",site_regr))
    out=lm(formula = form, data = merged_ordered)
    filename <- paste('lm_SANS_site_sex_age_MED',ROI,sep='_') 
    assign(filename, out)
  }
}else {
  cat('no MEDICATION or site variable')
}

cat('saving complex regression output','\n')
save(list=ls(pattern="lm_SANS_site*"),file="lm_SANS_site.RData")


################################ pos symp: run regression ###############################

if (posROIs[1] %in% names(merged_ordered) == FALSE) {
  stop('ROIs needed for positive symptom project not present')
}

cat('positive symptoms: running simple regressions','\n')

#CT=SAPS_GLOBAL+Age+Sex

if ("Sex" %in% names(merged_ordered) == TRUE & "Age" %in% names(merged_ordered) == TRUE){
  for (ROI in posROIs){
    #print(ROI)
    form <- as.formula(paste(ROI, "~SAPS_GLOBAL+Age+as.factor(Sex)"))
    out=lm(formula = form, data = merged_ordered)
    filename <- paste('lm_SAPS_sex_age',ROI,sep='_') 
    assign(filename, out)
  }
}else {
  cat('no sex or age variable')
}


if ("HAND" %in% names(merged_ordered) == TRUE){
  cat('adding HAND to regressions with age and sex','\n')
  for (ROI in posROIs){
    #print(ROI)
    form <- as.formula(paste(ROI, "~SAPS_GLOBAL+Age+as.factor(Sex)+as.factor(HAND)"))
    out=lm(formula = form, data = merged_ordered)
    filename <- paste('lm_SAPS_sex_age_hand',ROI,sep='_') 
    assign(filename, out)
  }
}else {
  cat('no HAND variable')
}


if ("LENGTH_OF_ILLNESS" %in% names(merged_ordered) == TRUE){
  cat('adding LENGTH_OF_ILLNESS to regressions with age and sex','\n')
  for (ROI in posROIs){
    #print(ROI)
    form <- as.formula(paste(ROI, "~SAPS_GLOBAL+Age+as.factor(Sex)+LENGTH_OF_ILLNESS"))
    out=lm(formula = form, data = merged_ordered)
    filename <- paste('lm_SAPS_sex_age_LOI',ROI,sep='_') 
    assign(filename, out)
  }
}else {
  cat('no LENGTH_OF_ILLNESS variable')
}

if ("PANSS_TOTAL" %in% names(merged_ordered) == TRUE){
  cat('adding PANSS_TOTAL (illness severity) to regressions with age and sex','\n')
  for (ROI in posROIs){
    #print(ROI)
    form <- as.formula(paste(ROI, "~SAPS_GLOBAL+Age+as.factor(Sex)+PANSS_TOTAL"))
    out=lm(formula = form, data = merged_ordered)
    filename <- paste('lm_SAPS_sex_age_IllSev',ROI,sep='_') 
    assign(filename, out)
  }
}else {
  cat('no PANSS_TOTAL variable')
}

if ("MEDICATION" %in% names(merged_ordered) == TRUE){
  cat('adding MEDICATION to regressions with age and sex','\n')
  for (ROI in posROIs){
    #print(ROI)
    form <- as.formula(paste(ROI, "~SAPS_GLOBAL+Age+as.factor(Sex)+as.factor(MEDICATION)"))
    out=lm(formula = form, data = merged_ordered)
    filename <- paste('lm_SAPS_sex_age_MED',ROI,sep='_') 
    assign(filename, out)
  }
}else {
  cat('no MEDICATION variable')
}

cat('saving simple regression output','\n')
save(list=ls(pattern="lm_SAPS*"),file="lm_SAPS_simple.RData")

############################################################################
################################ pos symp: run regression, with site ###############################

cat('positive symptoms: repeating regression models, this time including site','\n')

#CT=SAPS_GLOBAL+Age+Sex

site_regr=paste(colnames(merged_ordered)[(n.covs-n.sites+2):(n.covs+1)],collapse="+")

if ("Sex" %in% names(merged_ordered) == TRUE & "Age" %in% names(merged_ordered) == TRUE & n.sites != 0){
  for (ROI in posROIs){
    #print(ROI)
    form <- as.formula(paste(ROI, "~SAPS_GLOBAL+Age+as.factor(Sex)+",site_regr))
    out=lm(formula = form, data = merged_ordered)
    filename <- paste('lm_SAPS_site_sex_age',ROI,sep='_') 
    assign(filename, out)
  }
}else {
  cat('no sex, age or site variable')
}


if ("HAND" %in% names(merged_ordered) == TRUE & n.sites != 0){
  cat('adding HAND to regressions with age, sex and site','\n')
  for (ROI in posROIs){
    #print(ROI)
    form <- as.formula(paste(ROI, "~SAPS_GLOBAL+Age+as.factor(Sex)+as.factor(HAND)+",site_regr))
    out=lm(formula = form, data = merged_ordered)
    filename <- paste('lm_SAPS_site_sex_age_hand',ROI,sep='_') 
    assign(filename, out)
  }
}else {
  cat('no HAND or site variable')
}


if ("LENGTH_OF_ILLNESS" %in% names(merged_ordered) == TRUE & n.sites != 0){
  cat('adding LENGTH_OF_ILLNESS to regressions with age, sex and site','\n')
  for (ROI in posROIs){
    #print(ROI)
    form <- as.formula(paste(ROI, "~SAPS_GLOBAL+Age+as.factor(Sex)+LENGTH_OF_ILLNESS+",site_regr))
    out=lm(formula = form, data = merged_ordered)
    filename <- paste('lm_SAPS_site_sex_age_LOI',ROI,sep='_') 
    assign(filename, out)
  }
}else {
  cat('no LENGTH_OF_ILLNESS or site variable')
}

if ("PANSS_TOTAL" %in% names(merged_ordered) == TRUE & n.sites != 0){
  cat('adding PANSS_TOTAL (illness severity) to regressions with age, sex and site','\n')
  for (ROI in posROIs){
    #print(ROI)
    form <- as.formula(paste(ROI, "~SAPS_GLOBAL+Age+as.factor(Sex)+PANSS_TOTAL+",site_regr))
    out=lm(formula = form, data = merged_ordered)
    filename <- paste('lm_SAPS_site_sex_age_IllSev',ROI,sep='_') 
    assign(filename, out)
  }
}else {
  cat('no PANSS_TOTAL or site variable')
}

if ("MEDICATION" %in% names(merged_ordered) == TRUE & n.sites != 0){
  cat('adding MEDICATION to regressions with age, sex and site','\n')
  for (ROI in posROIs){
    #print(ROI)
    form <- as.formula(paste(ROI, "~SAPS_GLOBAL+Age+as.factor(Sex)+as.factor(MEDICATION)+",site_regr))
    out=lm(formula = form, data = merged_ordered)
    filename <- paste('lm_SAPS_site_sex_age_MED',ROI,sep='_') 
    assign(filename, out)
  }
}else {
  cat('no MEDICATION or site variable')
}

cat('saving complex regression output','\n')
save(list=ls(pattern="lm_SAPS_site*"),file="lm_SAPS_site.RData")


#############################

cat('finished!')

