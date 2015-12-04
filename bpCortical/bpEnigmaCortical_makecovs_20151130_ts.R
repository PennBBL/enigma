###LOAD DATA###
fullSample <- read.csv("/import/monstrum/enigma/ENIGMA_CORTICAL_BP_SZ/BP_ENIGMA/covariates/n287_day2_fndm_nodra_effrt.csv")


###SUBSET SAMPLE###

#remove all subjects except for bipolar and healthy controls
#note use of "masterTmp" so don't overwrite master 
#note use of grep so can select all bipolar cases at once
masterTmp<-fullSample[c(grep("bipolar",fullSample$primarydiagnosis), which(fullSample$primarydiagnosis=="noDiagnosis")),]

###BBLID_SCANID###
masterTmp <- within(masterTmp, bblid_scanid <- paste(bblid, scanid, sep='_'))


###SUBSET VARIABLES###

master <- masterTmp[,c("bblid","bblid_scanid","age_at_date_provided","sex","primarydiagnosis", "moodstate")] ##get only relevant columns from master, rename correctly
names(master)[names(master)=="age_at_date_provided"] <- "Age"
names(master)[names(master)=="sex"] <- "Sex_old"
names(master)[names(master)=="primarydiagnosis"] <- "FullDx_old"
names(master)[names(master)=="moodstate"] <- "MoodState_old" #made this so don't have to name each factor level below


###BINARY DX###

master$Dx <-NA #add Dx column, would make it empty to start rather than copying FullDx
master$Dx[grep("bipolar",master$FullDx_old)] <- 1  #use of grep cuts down on separate statements for each bipolar subtype; no need for quotes
master$Dx[which(master$FullDx_old == "noDiagnosis")] <- 0 ##added "which" statement here as otherwise can pickup NA's sometimes


###MOOD STATE###

#make mood state col
master$MoodState<-NA  #start w/ NA so don't have to add each level separately-- more concise

#convert the levels-- now indexed to MoodState_old; also added which statements throughout so don't get messed up by NA's
master$MoodState[which(master$MoodState_old == "Euthymic")] <- 1 #no need for quotes around this-- we want it numeric, not char
master$MoodState[which(master$MoodState_old == "Depressed")] <- 2
master$MoodState[which(master$MoodState_old == "Manic")] <- 3
master$MoodState[which(master$MoodState_old == "Hypomanic")] <- 4
master$MoodState[which(master$MoodState_old == "Mixed")] <- 5
#note that because col was started as NA, can leave the "unknown", the "rapid cycling", and the "uknown" [sic] as is-- they are NA as default; note that 11 subjects is **WAY** too much missing data-- aylin/sage need to get to the bottom of this.
master$MoodState[which(master$Dx==0)]<-NA  #note that several controls had a listed mood state; by definition they are euthymic-- this was an error in the input data.
#master$MoodState_old <- NULL #remove old mood state


###FULLL DX###

master$FullDx<-NA  #make new fullDx col, starts empty

#convert from old to new-- added which statement
master$FullDx[which(master$FullDx_old == "bipolarDisorderTypeI")] <- 1 #no need for quotes around this-- we want it numeric, not char
master$FullDx[which(master$FullDx_old == "bipolarDisorderTypeII")] <- 2
master$FullDx[which(master$FullDx_old == "bipolarDisorderNOS")] <- 3
master$FullDx_old <- NULL


##AGE###
master$Age<-round(master$Age) #round off-- worried about precision in merging

###SEX###
master$Sex<-NA
master$Sex[master$Sex_old == 1] <- 2   #no need for quotes around this-- we want it numeric, not char
master$Sex[master$Sex_old == 0] <- 1
master$Sex_old <- NULL


###MEDS###

#load in meds-- make temporary "In" files before you subset
day2_fndmIn <- read.csv("/import/monstrum/enigma/ENIGMA_CORTICAL_BP_SZ/BP_ENIGMA/covariates/day2_fndm_fndm2_meds_9-2-15.csv")
effortIn <- read.csv("/import/monstrum/enigma/ENIGMA_CORTICAL_BP_SZ/BP_ENIGMA/covariates/effort_meds_9-2-15_dw.csv")
nodraIn <- read.csv("/import/monstrum/enigma/ENIGMA_CORTICAL_BP_SZ/BP_ENIGMA/covariates/nodra_med_9-8-15.csv")

#get only necessary columns
day2_fndm <- day2_fndmIn[,c("BBLID","SCANID","typical.AP","atypical.AP","lithium", "mood.stabilizing.anticonv", "other.anticonvuls", "antidepressant")]
effort <- effortIn[,c("BBLID","SCANID","typical.AP","atypical.AP","lithium","mood.stabilizing.anticonv","other.anticonvuls","antidepressant")]
nodra <- nodraIn[,c("BBLID","SCANID","typical.AP","atypical.AP","lithium","mood.stabilizing.anticonv","other.anticonvuls","antidepressant")]


###RENAME MEDS###

#give columns correct names--- probably couldhave done this only once after merging rather than 3 times (once per project), but I left as is
names(day2_fndm)[names(day2_fndm)=="BBLID"] <- "bblid"
names(day2_fndm)[names(day2_fndm)=="SCANID"] <- "scanid"
names(day2_fndm)[names(day2_fndm)=="typical.AP"] <- "Gen1AntiPsych"
names(day2_fndm)[names(day2_fndm)=="atypical.AP"] <- "Gen2AntiPsych"
names(day2_fndm)[names(day2_fndm)=="lithium"] <- "Li"
names(day2_fndm)[names(day2_fndm)=="mood.stabilizing.anticonv"] <- "AntiEpileptic"
names(day2_fndm)[names(day2_fndm)=="other.anticonvuls"] <- "AntiEpileptic2"
names(day2_fndm)[names(day2_fndm)=="antidepressant"] <- "AntiDep"
day2_fndm <- within(day2_fndm, bblid_scanid <- paste(bblid, scanid, sep='_'))

names(effort)[names(effort)=="BBLID"] <- "bblid"
names(effort)[names(effort)=="SCANID"] <- "scanid"
names(effort)[names(effort)=="typical.AP"] <- "Gen1AntiPsych"
names(effort)[names(effort)=="atypical.AP"] <- "Gen2AntiPsych"
names(effort)[names(effort)=="lithium"] <- "Li"
names(effort)[names(effort)=="mood.stabilizing.anticonv"] <- "AntiEpileptic"
names(effort)[names(effort)=="other.anticonvuls"] <- "AntiEpileptic2"
names(effort)[names(effort)=="antidepressant"] <- "AntiDep"
effort <- within(effort, bblid_scanid <- paste(bblid, scanid, sep='_'))

names(nodra)[names(nodra)=="BBLID"] <- "bblid"
names(nodra)[names(nodra)=="SCANID"] <- "scanid"
names(nodra)[names(nodra)=="typical.AP"] <- "Gen1AntiPsych"
names(nodra)[names(nodra)=="atypical.AP"] <- "Gen2AntiPsych"
names(nodra)[names(nodra)=="lithium"] <- "Li"
names(nodra)[names(nodra)=="mood.stabilizing.anticonv"] <- "AntiEpileptic"
names(nodra)[names(nodra)=="other.anticonvuls"] <- "AntiEpileptic2"
names(nodra)[names(nodra)=="antidepressant"] <- "AntiDep"
nodra <- within(nodra, bblid_scanid <- paste(bblid, scanid, sep='_'))


###MERGE MEDS###

#merge the master file with the med info####
#get meds of only relevant subjects
#SubjID <- master$SubjID
#SubjID <- as.data.frame(SubjID)

#merge all of the meds
day2_effort <- merge(day2_fndm,effort,all=TRUE)
day2_effort_nodra <- merge(day2_effort,nodra,all=TRUE)

#merge meds with other covariates--- note can use "all.x=TRUE" to ensure that you only take the subjects from the "master" dataframe you are interested in
masterMeds <- merge(master, day2_effort_nodra,all.x=TRUE, by="bblid_scanid")

#fix bblid which was not present for some from med list
masterMeds$bblid<-masterMeds$bblid.x
masterMeds$bblid.y<-NULL
masterMeds$bblid.x<-NULL

#set NAs for controls-- I didn't quite follow the use of within here, so used which-- a bit more typical 
masterMeds$Li[which(masterMeds$Dx==0)]<-NA
masterMeds$Gen1AntiPsych[which(masterMeds$Dx==0)]<-NA
masterMeds$Gen2AntiPsych[which(masterMeds$Dx==0)]<-NA
masterMeds$AntiEpileptic[which(masterMeds$Dx==0)]<-NA
masterMeds$AntiEpileptic2[which(masterMeds$Dx==0)]<-NA
masterMeds$AntiDep[which(masterMeds$Dx==0)]<-NA


###OTHER DATA: AGE OF ONSET AND HX OF PSYCHOSIS###

#Load in additional data-- called "In" so don't overwrite, also called them "Bp" so don't confuse w/ objects used above for meds
fndmBpIn <- read.csv("/import/monstrum/enigma/ENIGMA_CORTICAL_BP_SZ/BP_ENIGMA/covariates/FNDM_bipolarquestions.csv")  
nodraBpIn <- read.csv("/import/monstrum/enigma/ENIGMA_CORTICAL_BP_SZ/BP_ENIGMA/covariates/NODRA_bipolarquestions.csv")
addBpIn <- read.csv("/import/monstrum/enigma/ENIGMA_CORTICAL_BP_SZ/BP_ENIGMA/covariates/MissingAgeOnset_Lauren.csv")

#get only necessary columns
fndmBp <- fndmBpIn[,c("bblid","age_onset","history_psychosis")]
nodraBp <- nodraBpIn[,c("bblid","age_onset_mood_dx","history_psychosis")]
addBp <- addBpIn[,c("bblid","age_onset","history_psychosis")]

#rename correctly
names(fndmBp)[names(fndmBp)=="age_onset"] <- "AgeofOnset"
names(fndmBp)[names(fndmBp)=="history_psychosis"] <- "HistoryPsychosis"

names(nodraBp)[names(nodraBp)=="age_onset_mood_dx"] <- "AgeofOnset"
names(nodraBp)[names(nodraBp)=="history_psychosis"] <- "HistoryPsychosis"

names(addBp)[names(addBp)=="age_onset"] <- "AgeofOnset"
names(addBp)[names(addBp)=="history_psychosis"] <- "HistoryPsychosis"

#merge AgeofOnset, HistoryPsychosis
fndm_nodra_bp <- merge(fndmBp,nodraBp,all=TRUE)
fndm_nodra_add_bp <- merge(fndm_nodra_bp,addBp,all=TRUE)
fndm_nodra_add_bp$HistoryPsychosis[which(fndm_nodra_add_bp$HistoryPsychosis == "y")] <- 1
fndm_nodra_add_bp$HistoryPsychosis[which(fndm_nodra_add_bp$HistoryPsychosis == "n")] <- 0

#Remove duplicated variables
fndm_nodra_add_bp_subset<-fndm_nodra_add_bp[fndm_nodra_add_bp$bblid %in% masterMeds$bblid,]
fndm_nodra_add_bp_subset_unique <- fndm_nodra_add_bp_subset[!duplicated(fndm_nodra_add_bp_subset$bblid),]  #I checked this at length to make sure was keeping duplicated rows with values, not NAs

#merge these extra variables with existing dataframe
masterMedsAdd <- merge(masterMeds,fndm_nodra_add_bp_subset_unique,all=TRUE,by="bblid") 


###Consolidate AntiEpileptic column####
masterMedsAdd$AntiEp <- masterMedsAdd$AntiEpileptic + masterMedsAdd$AntiEpileptic2
masterMedsAdd$AntiEpileptic <- NULL
masterMedsAdd$AntiEpileptic2 <- NULL
names(masterMedsAdd)[names(masterMedsAdd)=="AntiEp"] <- "AntiEpileptic"
masterMedsAdd$AntiEpileptic[which(masterMedsAdd$AntiEpileptic == 2)] <- 1
masterMedsAdd$AntiEpileptic[which(masterMedsAdd$Dx == 0) ] <- NA


###FINAL FORMATTING###
masterMedsAdd$SubjID <- masterMedsAdd$bblid
masterMedsAdd$scanid <- NULL
masterMedsAdd$bblid<- NULL

scanid_bblid<-masterMedsAdd$bblid_scanid  #save this as a separate object and write out
masterMedsAdd$bblid_scanid<-NULL

masterFinal <- masterMedsAdd[,c("SubjID", "Dx", "Age", "Sex", "FullDx", "Li", "AntiEpileptic", "Gen1AntiPsych", "Gen2AntiPsych", "AntiDep", "MoodState", "AgeofOnset", "HistoryPsychosis")]


####check for duplicated rows
sum(duplicated(masterFinal$SubjID))


###WRITE OUT FINAL FILE###
write.csv(masterFinal, file="/import/monstrum/enigma/ENIGMA_CORTICAL_BP_SZ/BP_ENIGMA/covariates/n146_bpEnigmaCortical_covariates_2015129.csv",row.names=FALSE,quote=FALSE)
