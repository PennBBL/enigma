##############################################################################
################                                               ###############
################             ENIGMA Covariates File            ###############
################                 Lauren Beard                  ###############
################                  03/29/2016                  ###############
##############################################################################


##############################################################################
################               Read in raw files               ###############
##############################################################################
subjects <- read.csv("/import/monstrum/enigma/cortical/sz/covariates/n370_enigma.csv")
demos <- read.csv("/import/monstrum/enigma/cortical/sz/covariates/adminnew.csv")
sansRaw <- read.csv("/import/monstrum/enigma/cortical/sz/covariates/sans.csv")
sapsRaw <- read.csv("/import/monstrum/enigma/cortical/sz/covariates/saps.csv")
meds <- read.csv("/import/monstrum/enigma/cortical/sz/covariates/medicine.csv")
status_ageonset <- read.csv("/import/monstrum/enigma/cortical/sz/covariates/status_ageonset.csv")
diagnosis <- read.csv("/import/monstrum/enigma/cortical/sz/covariates/diagnosis.csv")
wrat <- read.csv("/import/monstrum/enigma/cortical/sz/covariates/bblid_iq.csv")
site <- read.csv("/import/monstrum/enigma/cortical/sz/covariates/site.csv")

### Create basic dataset
temp <- merge(subjects, status_ageonset, by="BBLID",all=TRUE)

### Clean columns and change missing values to NA
temp$AGEONSET[which(temp$AGEONSET==".")] <- NA
temp$CSTATUS[which(temp$CSTATUS==".")] <- NA
temp$ISTATUS <- NULL

### Remove bipolar subjects and other psychiatric diagnosis from the sample
temp$CSTATUS[which(temp$CSTATUS== "BP1")] <- NA
temp$CSTATUS[which(temp$CSTATUS== "BP2")] <- NA
temp$CSTATUS[which(temp$CSTATUS== "O")] <- NA
temp <- temp[complete.cases(temp$CSTATUS),]

### Angel: You have 1 CB 1 FC 192 NC, is that right? yes.

### Fix odd subject (marked as NC in oracle, ageonset also listed in oracle)
temp$CSTATUS <- as.character(temp$CSTATUS)
temp$CSTATUS[which(temp$BBLID=="12217")] <- "SZ"
temp$CSTATUS <- as.factor(temp$CSTATUS)

### Create DX column
temp$DX <- temp$CSTATUS



### Standardize CStatus for Enigma protocol purposes
levels(temp$DX) <- c(levels(temp$DX), 0,1, NA)
temp$DX[which(temp$DX== "NC")] <- 0
temp$DX[which(temp$DX== "CB")] <- 0
temp$DX[which(temp$DX== "FC")] <- 0
temp$DX[which(temp$DX== "S")] <- 1
temp$DX[which(temp$DX== "SAD")] <- 1
temp$DX[which(temp$DX== "SAM")] <- 1
temp$DX[which(temp$DX== "SX")] <- 1
temp$DX[which(temp$DX== "SZ")] <- 1

##############################################################################
################          GET SANS  closest to DOSCAN          ###############
##############################################################################

#### Calculate SANSTOT according to Enigma
sansRaw$SANSTOT <- sansRaw$SANS1 + sansRaw$SANS2 + sansRaw$SANS3 + sansRaw$SANS4 + sansRaw$SANS5 + sansRaw$SANS6 + sansRaw$SANS7 + sansRaw$SANS9 + sansRaw$SANS10 + sansRaw$SANS11+ sansRaw$SANS12 + sansRaw$SANS14 + sansRaw$SANS15 + sansRaw$SANS16 + sansRaw$SANS18 + sansRaw$SANS19 + sansRaw$SANS20 + sansRaw$SANS21

#### Angel: You don't add all of them, why? according to ENIGMA protocol

sansInt <- merge(subjects, sansRaw, all=TRUE)


### Calculate the number of days between desired scan and all subject dates given
sansInt$dosans <- as.Date(sansInt$DOSANS, format="%m/%d/%Y")
sansInt$doscan <- as.Date(sansInt$DOSCAN, format="%m/%d/%y")
sansInt$days <- difftime(sansInt$dosans, sansInt$doscan, units = c("days"))

### Select sans scores that are closest to the date of scan
### Must load plyr first (for meds later)
require(plyr)

### Load dplyr for sans/saps
require("dplyr")

### Create new dataset with SANS score closets to scan
midSans <- sansInt %>% 
  group_by(BBLID, doscan) %>% 
  slice(which.min(abs(doscan - dosans)))
midSans <- as.data.frame(midSans)

### Angel: Never used this before but we wrote loops to compare output and it is correct

fixSans <- midSans[,c("BBLID", "SCANID", "SANSTOT")]

### Merge in lost bblids- those who don't have sans scores
sansFull <- merge(fixSans, subjects, by=c("BBLID","SCANID"), all=TRUE)

##############################################################################
################          GET SAPS  closest to DOSCAN          ###############
##############################################################################


sapsRaw$SAPS28 <- as.integer(as.character(sapsRaw$SAPS28))
sapsRaw$SAPSTOT <- sapsRaw$SAPS1 + sapsRaw$SAPS2 + sapsRaw$SAPS3 + sapsRaw$SAPS4 + sapsRaw$SAPS5 + sapsRaw$SAPS6 + sapsRaw$SAPS8 + sapsRaw$SAPS9 + sapsRaw$SAPS10 + sapsRaw$SAPS11 + sapsRaw$SAPS12 + sapsRaw$SAPS13 + sapsRaw$SAPS14 + sapsRaw$SAPS15 + sapsRaw$SAPS16 + sapsRaw$SAPS17 + sapsRaw$SAPS18 + sapsRaw$SAPS19 + sapsRaw$SAPS21 + sapsRaw$SAPS22 + sapsRaw$SAPS23 + sapsRaw$SAPS24 + sapsRaw$SAPS26 + sapsRaw$SAPS27 + sapsRaw$SAPS28 + sapsRaw$SAPS29 + sapsRaw$SAPS30 + sapsRaw$SAPS31 + sapsRaw$SAPS32 + sapsRaw$SAPS33

sapsInt <- merge(sansFull, sapsRaw, all=TRUE)

#### Calculate the number of days between desired scan and all subject dates given
sapsInt$dosaps <- as.Date(sapsInt$DOSAPS, format="%m/%d/%Y")
sapsInt$doscan <- as.Date(sapsInt$DOSCAN, format="%m/%d/%y")
sapsInt$days <- difftime(sapsInt$dosaps, sapsInt$doscan, units = c("days"))

### Select saps scores that are closest to the date of scan
require(dplyr)

### Angel: Never done this before but we wrote a loop and it works
midSaps <- sapsInt %>% 
  group_by(BBLID, doscan) %>% 
  slice(which.min(abs(doscan - dosaps)))
midSaps <- as.data.frame(midSaps)

fixSaps <- midSaps[,c("BBLID", "SCANID", "SAPSTOT")]

### Merge in lost bblids- those who don't have sans scores
fullSansSaps <- merge(fixSaps, sansFull, by=c("BBLID","SCANID"), all = TRUE)


### Merge with temporary dataset
temp <- merge(temp, fullSansSaps, by=c("BBLID","SCANID","DOSCAN"))


##############################################################################
################          GET Basic Demographics               ###############
##############################################################################


temp <- merge(temp, demos, by="BBLID")

### Standarize Columns
temp$HAND[which(temp$HAND== 1 )] <- 0
temp$HAND[which(temp$HAND== 2 )] <- 1
temp$HAND[which(temp$HAND== 3 )] <- 2
temp$ETHNIC[which(temp$ETHNIC== "." )] <- NA
temp$RACE[which(temp$RACE== "." )] <- NA
temp$MOM_EDUC[which(temp$MOM_EDUC== "." )] <- NA
temp$DAD_EDUC[which(temp$DAD_EDUC== "." )] <- NA


### Set SAPS / SANS as NA for Dx == 0 (controls)
temp$SANSTOT[which(temp$DX== 0 )] <- "NA"
temp$SAPSTOT[which(temp$DX== 0 )] <- "NA"

### Create site column
temp$PROTOCOL <- NULL
site <- unique(site)
temp <- merge(temp, site, by=c("BBLID","SCANID"))
temp$Site <- 0
temp$Site[which(temp$PROTOCOL== "808922 - MGI2_PENN")] <- 1

### Old Code here
#standardize cstatus
#levels(temp$DX) <- c(levels(temp$DX), 0,1, NA)
#temp$DX[which(temp$DX== "NC")] <- 0
#temp$DX[which(temp$DX== "CB")] <- 0
#temp$DX[which(temp$DX== "FC")] <- 0
#temp$DX[which(temp$DX== "S")] <- 1
#temp$DX[which(temp$DX== "SAD")] <- 1
#temp$DX[which(temp$DX== "SAM")] <- 1
#temp$DX[which(temp$DX== "SX")] <- 1
#temp$DX[which(temp$DX== "O")] <- 1


##############################################################################
################       Get Medicine and CPZ Information        ###############
##############################################################################

### Get only antipsychotics
medsTemp <- meds[c(grep("ANTIPSYCHOTIC-PO",meds$PHARMCLASS), which(meds$PHARMCLASS=="ANTIPSYCHOTIC-DECANOATE"), which(meds$PHARMCLASS=="Antipsychotics, Atypical, Dopamine and Serotonin Antagonists"), which(meds$PHARMCLASS=="Atypical Antipsychotic [EPC]"), which(meds$PHARMCLASS=="Atypical Antipsychotic [EPC], Atypical Antipsychotic [EPC]")),]

### Merge with list of subjects
medsTemp <- merge(medsTemp, subjects, by="BBLID", all=TRUE)


### Remove meds that aren't being taken during doscan
medsTemp$DOMED_START <- as.Date(medsTemp$DOMED_START, format="%m/%d/%Y")
medsTemp$DOMED_END <- as.POSIXlt(as.character(medsTemp$DOMED_END), format="%m/%d/%Y")
medsTemp$doscan <- as.POSIXlt(as.character(medsTemp$DOSCAN), format="%m/%d/%y")
medsTemp$days <- difftime(medsTemp$DOMED_END, medsTemp$doscan, units = c("days"))
medsTemp$days2 <- difftime(medsTemp$DOMED_START, medsTemp$doscan, units = c("days"))

medsTemp <- medsTemp[!is.na(medsTemp$days2),]

medsTemp <- medsTemp[medsTemp$days2 < -1,]
medsTemp$days2 <- medsTemp$days2 * -1

medsTemp <- medsTemp[medsTemp$days > -1,]

medsTemp <- medsTemp[!is.na(medsTemp$BBLID),]


### Old code still here for tracking purposes

#medsTemp$minday <- pmin(as.numeric(medsTemp$days), as.numeric(medsTemp$days2))
#medsTemp <- medsTemp[ medsTemp$days == ave(medsTemp$minday, medsTemp$BBLID, FUN=min), ]
### Skip ahead all of this
### 
#bblid.list <- unique(medsTemp$BBLID)
#bblid.list <- bblid.list[complete.cases(bblid.list)]
#index.final <- 0

#for (i in bblid.list) {
#  smallest <- as.numeric(min(medsTemp$days[medsTemp$BBLID == i], na.rm = T))
#  #print(i)
#  #print(smallest)
#  index.final <- c(index.final, which(as.numeric(medsTemp$days) == smallest & medsTemp$BBLID == i))
#}

#index.final <- index.final[-1]

#meds.Temp.angel <- medsTemp[index.final, ]

#choose the info closest to end of meds
#medsInt <- meds[c(grep("ANTIPSYCHOTIC-PO",meds$PHARMCLASS), which(meds$PHARMCLASS=="ANTIPSYCHOTIC-DECANOATE"), which(meds$PHARMCLASS=="Antipsychotics, Atypical, Dopamine and Serotonin Antagonists"), which(meds$PHARMCLASS=="Atypical Antipsychotic [EPC]"), which(meds$PHARMCLASS=="Atypical Antipsychotic [EPC], Atypical Antipsychotic [EPC]")),]

#medsInt <- merge(medsInt, subjects, by="BBLID", all=TRUE)
#maybe the one that is closest to the start date?
#calculate the number of days between desired scan and all subject dates given
#medsInt$DOMED_START <- as.Date(medsInt$DOMED_START, format="%m/%d/%Y")
#medsInt$doscan <- as.Date(medsInt$DOSCAN, format="%m/%d/%y")
#medsInt$days <- difftime(medsInt$DOMED_START, medsInt$doscan, units = c("days"))
#medsInt <- medsInt[medsInt$days < 1,]
#medsInt <- medsInt[ medsInt$days == ave(medsInt$days, medsInt$BBLID, FUN=min), ]
#medsInt <- medsInt[!is.na(medsInt$BBLID),]
#medsInt$days <- medsInt$days * -1

#choose overall med info that is closest to DOSCAN
#medsTemp <- medsTemp[,c("BBLID", "MEDICINE", "PHARMCLASS", "DOSAGE","days")]
#medsInt <- medsInt[,c("BBLID", "MEDICINE", "PHARMCLASS", "DOSAGE","days")]
#medsTotal <- merge(medsTemp, medsInt, by=c("BBLID", "MEDICINE", "PHARMCLASS", "DOSAGE","days"), all=TRUE)
#medsTotal <- medsTotal[ medsTotal$days == ave(medsTotal$days, medsTotal$BBLID, FUN=min), ]
#medsTotal <- medsTotal[!is.na(medsTotal$days)]


### Create MedsTotal Dataset

medsTotal <- medsTemp[,c("BBLID", "MEDICINE", "PHARMCLASS", "DOSAGE")]


### Create CPZ Equivalent
medsTotal$DOSAGE <- as.numeric(as.character(medsTotal$DOSAGE))

### remove doses of 999/9999 
medsTotal$DOSAGE[which(medsTotal$DOSAGE== 999)] <- NA
medsTotal$DOSAGE[which(medsTotal$DOSAGE== 9999)] <- NA
medsTotal <- medsTotal[complete.cases(medsTotal),]


### Save old dosage for comparison Purposes
medsTotal$oldDosage <- medsTotal$DOSAGE

#### CPZ Conversions
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="ABILIFY             ")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="ABILIFY             ")]*(100/7.5)
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="ABILIFY")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="ABILIFY")]*(100/7.5)
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="CLOZARIL")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="CLOZARIL")]*(100/50)
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="CLOZARIL            ")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="CLOZARIL            ")]*(100/50)
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="GEODON")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="GEODON")]*(100/60)
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="HALDOL (DEC)        ")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="HALDOL (DEC)        ")]*(100/30)
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="HALDOL (PO)         ")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="HALDOL (PO)         ")]*(100/2)
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="LOXITANE            ")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="LOXITANE            ")]*(100/10)
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="NAVANE              ")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="NAVANE              ")]*(100/4)
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="PROLIXIN (DEC)      ")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="PROLIXIN (DEC)      ")]*(100/2.5)
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="PROLIXIN (PO)       ")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="PROLIXIN (PO)       ")]*(100/2)
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="RISPERDAL           ")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="RISPERDAL           ")]*(100/2)
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="RISPERDAL")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="RISPERDAL")]*(100/2)
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="RISPERIDONE")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="RISPERIDONE")]*(100/2)
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="SEROQUEL")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="SEROQUEL")]*(100/75)
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="SEROQUEL")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="SEROQUEL")]*(100/75)
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="SEROQUEL            ")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="SEROQUEL            ")]*(100/75)
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="STELAZINE           ")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="STELAZINE           ")]*(100/2)
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="THORAZINE           ")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="THORAZINE           ")]*(100/100)
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="TRILAFON            ")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="TRILAFON            ")]*(100/8)
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="ZIPRASIDONE         ")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="ZIPRASIDONE         ")]*(100/60)
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="ZYPREXA")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="ZYPREXA")]*(100/5)
medsTotal$DOSAGE[which(medsTotal$MEDICINE=="ZYPREXA             ")] <- medsTotal$DOSAGE[which(medsTotal$MEDICINE=="ZYPREXA             ")]*(100/5)
names(medsTotal)[names(medsTotal)=="DOSAGE"] <- "CPZ"
medsTotal$CPZ <- round(medsTotal$CPZ,digits=2)

#remove study med
#medsTotal <- medsTotal[!(medsTotal$MEDICINE=="STUDY MED           "),]


### Write out intermediate meds file for sanity check
#write.csv(medsTotal, file = "/import/monstrum/enigma/cortical/sz/covariates/ENIGMA_medsCPZ_03302016.csv", row.names=FALSE)

medsTotal <- read.csv("/import/monstrum/enigma/cortical/sz/covariates/ENIGMA_medsCPZ_03312016_TS_FINAL_CLEAN.csv")

### Standardize med types: 
### Schizophrenia unmedicated=1 
### Schizophrenia on atypical=2 
### Schizophrenia on typical=3
### Schizophrenia on both atypical and typical=4 


levels(medsTotal$MEDICINE) <- c(levels(medsTotal$MEDICINE), 0,1,2,3)
levels(medsTotal$MEDICINE)[match("ABILIFY             ",levels(medsTotal$MEDICINE))] <- 2
levels(medsTotal$MEDICINE)[match("ABILIFY",levels(medsTotal$MEDICINE))] <- 2
levels(medsTotal$MEDICINE)[match("CLOZARIL",levels(medsTotal$MEDICINE))] <- 2
levels(medsTotal$MEDICINE)[match("CLOZARIL            ",levels(medsTotal$MEDICINE))] <- 2
levels(medsTotal$MEDICINE)[match("GEODON",levels(medsTotal$MEDICINE))] <- 2
levels(medsTotal$MEDICINE)[match("HALDOL (PO)         ",levels(medsTotal$MEDICINE))] <- 3
levels(medsTotal$MEDICINE)[match("HALDOL (DEC)        ",levels(medsTotal$MEDICINE))] <- 3
levels(medsTotal$MEDICINE)[match("LOXITANE            ",levels(medsTotal$MEDICINE))] <- 3
levels(medsTotal$MEDICINE)[match("NAVANE              ",levels(medsTotal$MEDICINE))] <- 3
levels(medsTotal$MEDICINE)[match("PROLIXIN (DEC)      ",levels(medsTotal$MEDICINE))] <- 3
levels(medsTotal$MEDICINE)[match("PROLIXIN (PO)       ",levels(medsTotal$MEDICINE))] <- 3
levels(medsTotal$MEDICINE)[match("RISPERDAL           ",levels(medsTotal$MEDICINE))] <- 2
levels(medsTotal$MEDICINE)[match("RISPERDAL",levels(medsTotal$MEDICINE))] <- 2
levels(medsTotal$MEDICINE)[match("RISPERIDONE",levels(medsTotal$MEDICINE))] <- 2
levels(medsTotal$MEDICINE)[match("SEROQUEL",levels(medsTotal$MEDICINE))] <- 2
levels(medsTotal$MEDICINE)[match("SEROQUEL            ",levels(medsTotal$MEDICINE))] <- 2
levels(medsTotal$MEDICINE)[match("STELAZINE           ",levels(medsTotal$MEDICINE))] <- 3
levels(medsTotal$MEDICINE)[match("THORAZINE           ",levels(medsTotal$MEDICINE))] <- 3
levels(medsTotal$MEDICINE)[match("TRILAFON            ",levels(medsTotal$MEDICINE))] <- 3
levels(medsTotal$MEDICINE)[match("ZIPRASIDONE         ",levels(medsTotal$MEDICINE))] <- 2
levels(medsTotal$MEDICINE)[match("ZYPREXA             ",levels(medsTotal$MEDICINE))] <- 2
levels(medsTotal$MEDICINE)[match("ZYPREXA",levels(medsTotal$MEDICINE))] <- 2
medsTotal$MEDICINE <- as.factor(as.character(medsTotal$MEDICINE))


### Remove unnecessary column PHARMCLASS, oldDosage
medsTotal$PHARMCLASS <- NULL
medsTotal$oldDosage <- NULL

### Add care of subjects taking multiple meds
medsTotal$MEDICINE <- as.numeric(as.character(medsTotal$MEDICINE))
### Add based on bblid 
### Angel: I have never used ddply before but we wrote a loop and this is correct
medsTotal <- ddply(medsTotal, "BBLID", summarize, CPZ = sum(CPZ), MEDICINE = sum(MEDICINE))

### Fix the merged medicine binary
medsTotal$MEDICINE[which(medsTotal$MEDICINE==4)] <- 2
medsTotal$MEDICINE[which(medsTotal$MEDICINE==5)] <- 4
medsTotal$MEDICINE[which(medsTotal$MEDICINE==7)] <- 4

### Combine Meds with temp datset (demos/sans/saps)
temp <- merge(temp, medsTotal, by="BBLID", all.x=T)
#temp <- merge(temp, cat, by=c("BBLID","SCANID","DOSCAN","CSTATUS","AGEONSET","DX","SAPSTOT","SANSTOT","SEX","ETHNIC","HAND","RACE","MOM_EDUC","DAD_EDUC","DOBIRTH","PROTOCOL","Site"), all=TRUE)


##############################################################################
################                 Calculate Age                 ###############
##############################################################################

temp$dobirth <- as.POSIXlt(as.character(temp$DOBIRTH), format="%m/%d/%Y")
temp$doscan <- as.POSIXlt(as.character(temp$DOSCAN), format="%m/%d/%y")
temp$Age <- difftime(temp$doscan, temp$dobirth, units = c("days"))
temp$Age <- temp$Age/365
temp$Age <- round(temp$Age, digits = 1)
temp$dobirth <- NULL
temp$doscan <- NULL

##############################################################################
################        Calculate Duration of Illnesss         ###############
##############################################################################

temp$Age <- as.numeric(temp$Age)
temp$AGEONSET <- as.numeric(as.character(temp$AGEONSET))
temp$DURILL <- temp$Age - temp$AGEONSET


##############################################################################
################          get current psych diagnosis          ###############
##############################################################################

### You might not need this

#choose dodiagnosis closest to doscan
#diag <- merge(subjects, diagnosis, by="BBLID",all=TRUE)
#diag <- unique(diag)

#calculate the number of days between desired scan and all subject dates given
#diag$dodiagnosis <- as.Date(diag$DODIAGNOSIS, format="%m/%d/%Y")
#diag$doscan <- as.Date(diag$DOSCAN, format="%m/%d/%y")
#diag <- diag[complete.cases(diag),]
#diag$days <- difftime(diag$dodiagnosis, diag$doscan, units = c("days"))


##### LOOP To find best diagnosis 
#bblids <- unique(diag$BBLID)[complete.cases(unique(diag$BBLID))]
#index.diag <- 0

#for (i in bblids) {
#  min <- min(abs(as.numeric(diag$days[diag$BBLID == i])))
#  index.diag <- c(index.diag, which(as.numeric(abs(diag$days)) == min & diag$BBLID == i))
#}

#index.diag <- index.diag[-1]

#diag <- diag[index.diag, ]

#select dodiagnosis closest to the date of scan
#diag <- diag %>% 
#  group_by(BBLID, doscan) %>% 
#  slice(which.min(abs(doscan - dodiagnosis)))
#diag <- as.data.frame(diag)

#remove columns
#diag <- diag[, c("BBLID","AXIS1_DX1")]
#cat <- merge(temp, diag, by="BBLID", all.x=T)
#cat <- unique(cat)

#standardize according to enigma 
temp$AXIS1_DX1[which(temp$CSTATUS== "S" )] <- 295.9
temp$AXIS1_DX1[which(temp$CSTATUS== "SX" )] <- 295.9
temp$AXIS1_DX1[which(temp$CSTATUS== "SZ" )] <- 295.9
temp$AXIS1_DX1[which(temp$CSTATUS== "SAD" )] <- 295.7
temp$AXIS1_DX1[which(temp$CSTATUS== "SAM" )] <- 295.7
temp$AXIS1_DX1[which(temp$CSTATUS== "NC" )] <- NA
temp$AXIS1_DX1[which(temp$CSTATUS== "CB" )] <- NA
temp$AXIS1_DX1[which(temp$CSTATUS== "FC" )] <- NA

#temp <- merge(cat,temp,by=c("BBLID","SCANID","DOSCAN","PROTOCOL","Site","CSTATUS","AGEONSET","DX","SAPSTOT","SANSTOT","SEX","ETHNIC","HAND","RACE","MOM_EDUC","DAD_EDUC","DOBIRTH","PROTOCOL","CPZ","MEDICINE","Age","DURILL"),all=TRUE)


##############################################################################
################            get IQ from WRAT Data              ###############
##############################################################################

temp <- merge(temp, wrat, by="BBLID", all.x=T)
#temp <- merge(cat, temp, by=c("BBLID","SCANID","DOSCAN","PROTOCOL","Site","CSTATUS","AGEONSET","DX","SAPSTOT","SANSTOT","SEX","ETHNIC","HAND","RACE","MOM_EDUC","DAD_EDUC","DOBIRTH","PROTOCOL","CPZ","MEDICINE","Age","DURILL","AXIS1_DX1"),all=TRUE)

####### rename columns #######
names(temp)[names(temp)=="SCANID"] <- "SubjID"
names(temp)[names(temp)=="DX"] <- "Dx"
names(temp)[names(temp)=="SEX"] <- "Sex"
names(temp)[names(temp)=="AGEONSET"] <- "AO"
names(temp)[names(temp)=="MEDICINE"] <- "AP"
names(temp)[names(temp)=="MOM_EDUC"] <- "PARENTSES"
names(temp)[names(temp)=="ETHNIC"] <- "ETHNICITY"
names(temp)[names(temp)=="AXIS1_DX1"] <- "CUR_PSY_DIS_DIAG"


####### Standarized Controls for Enigma 
#temp$CPZ[which(temp$DX== 0 )] <- "NA"
#temp$AP[which(temp$DX== 0 )] <- "0"


####### remove columns ####### 
temp$DOBIRTH <- NULL
temp$DOSCAN <- NULL
temp$BBLID <- NULL
temp$PROTOCOL <- NULL
temp$CSTATUS <- NULL
temp$RACE <- NULL
temp$DAD_EDUC <- NULL

#fix column order
temp <- temp[c("SubjID", "Dx","Age","Sex","Site","AP","CPZ","AO","DURILL","SAPSTOT","SANSTOT","HAND","PARENTSES","IQ","ETHNICITY","CUR_PSY_DIS_DIAG")]

####### Standarized Controls for Enigma 
temp$CPZ[which(temp$Dx== 0 )] <- NA
temp$SAPSTOT[which(temp$Dx== "0" )] <- NA
temp$SANSTOT[which(temp$Dx== "0" )] <- NA
temp$AP[which(temp$Dx== 0 )] <- 0

#fix cur_psy_dis_diag (some missing)
#temp$CUR_PSY_DIS_DIAG[which(temp$SubjID== "2061" )] <- 295.9
#temp$CUR_PSY_DIS_DIAG[which(temp$SubjID== "834" )] <- 295.9
#temp$CUR_PSY_DIS_DIAG[which(temp$SubjID== "1361" )] <- 295.9
#temp$CUR_PSY_DIS_DIAG[which(temp$SubjID== "2205" )] <- 295.9
#temp$CUR_PSY_DIS_DIAG[which(temp$SubjID== "4683" )] <- 295.9

# If there is a patient with no med data, then AP = NA
temp$AP[is.na(temp$AP)] <- "NA"
#temp$AP[is.na(temp$CPZ) & (temp$Dx == 1)] <- 1
names(temp)[which(names(temp) == "Site")] <- "Site1"

temp$PANSSTOT	<- ""
temp$PANSSPOS	<- ""
temp$PANSSNEG <- ""

temp <- temp[, c(1:9,17:19,10:14)]

####### write to final csv ####### 
write.csv(temp, file = "/import/monstrum/enigma/cortical/sz/covariates/n370_ENIGMA_covariates_20160524.csv", row.names=FALSE)
