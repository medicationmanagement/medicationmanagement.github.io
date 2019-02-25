################################################################################################
################### Calculating Drug-drug interactions using claims data #######################
################################################################################################
# The R codes below can be applied to claims databases or other medical databases to calculate #
#buren of the drug-drug interactions (DDIs), incuding 1) having DDIs; 2) number of DDIs; &3)dur#
#ation of DDIs.    
#
# Please cite the the website and article below when using these codes.
#
#Feng X, Zhang Y.Calculating drug-drug interactions using administrative data.
#Available at https://medicationmanagement.github.io/DDIcodes.html
#
#Feng X, Sambamoorthia U, Innes K, Castelli G, LeMasters T, Xiong L, Williams, MU, Tan X. 
#Predictors of Major Bleeding Among Working-Age Adults with Atrial Fibrillation: Evaluating
#the Effects of Potential Drug-drug Interactions and Switching from Warfarin to Non-vitamin
#K Oral Anticoagulants.Cardiovascular Drugs and Therapy.2018;32(6):591-600
#
# The following R codes are ready to use in R package (function names: drugs_day & drugs_day2)
# To allow the use of R package, the inputs below should be prepared.
#
#drugs_day2(input1,input2,input5,drugs_day( input1,input2,input3,input4),input6) 
#input 1: number of patients in the cohort;
#input 2: number of day (# of column in the matrix & > assessment period);
#input 3: number of all drug-drug interactions for the study drug; 
#input 4: drugs files directory: all other drugs profiles except study drug (cvs files)-including
#service date[variable name: datenumfill], patient id [variable name: id], days of supply[variable name:dayssup]; 
#input 5: study drug file directory: study drug profile (cvs file)- including service date[variable
#name:datenumfill], patient id [variable name: id], days of supply[variable name:dayssup]; 
#input 6: assessment time frame for drug-drug interaction (e.g., 30 days, 6 months)
#
#Example: drugs_day2(4120,1000,drugtaget,drugs_day(4120,1000,9,list.files()),600)
#Input 1 (n_pat): 4120 patients in the cohort;
#Input 2 (n_day): 1000 days created (> the assessment days+ days of supply);
#Input 3 (n_drug): 9 pairs of DDIs: the study drug can interact with 9 other drugs;
#Input 4 (files): list.files() is the directory of 9 excel file (csv) of the profile of the 9 drugs;
#Input 5 (drugtarget): is the directory of the study drug excel file (csv);
#Input 6 (time): the assessment time for potential DDI exposure is 600 days.

drugs_day <- function(n_pat, n_day, n_drug, files){
  n_drug_day <- matrix(0, ncol = n_day, nrow = n_pat)
  for (k in 1: n_drug){
    day_on_drug <- matrix(0, ncol = n_day, nrow = n_pat)
    drug_supply_refill <- read.csv(files[k])
    for (i in 1:n_pat) {
      # identify individual patient #
      drug_supply_refill_pat <- drug_supply_refill[drug_supply_refill$id==i,]
      n_record <- nrow(drug_supply_refill_pat)
      for (j in 1:n_record){
        subdata <- drug_supply_refill_pat[drug_supply_refill_pat$seq_id==j,]
        first <- subdata$datenumfill
        last <- subdata$datenumfill+subdata$dayssup-1
        day_on_drug[i,first:last] <- 1
      }
    }
    n_drug_day <- n_drug_day + day_on_drug
    return(day_on_drug)
  }
}

drugs_day2 <- function(n_pat,n_day,drugtaget,n_drug_day,time){
  #step2#the drug that could interact with the study drug
  drug<-data.frame(matrix(0, ncol =n_day, nrow = n_pat))
  ###################################
  #need input 5: drugtaget is the file of the base drug that require data input
  ###################################
  for (j in 1:n_pat) {
    subkd<-drugtaget[drugtaget$id==j,]
    leng<-length(subkd$id)*1
    for (i in 1:leng){
      subdata <-  subkd[ subkd$seq_id==i,]
      firsta <- subdata$datenumfill
      lasta <- subdata$datenumfill+subdata$dayssup-1
      drug[j,firsta:lasta]<-1
    }
  }
  
  #number of warfarin days covered.
  ddi_ccu<-as.data.frame(t(ddi_ccu ))
  colMax <- function(data) apply(data, 2, max)
  ddi_ccu<-colMax(ddi_ccu)
  ddi_ccu<-as.matrix(ddi_ccu)
  mean(ddi_ccu)
  #modify the study drug to create a new matrix, assiging 100 to the day with study drug;
  drug[drug < 1] <-0 
  drug[drug > 0] <-100
  #base drug + potential ddi frame;
  ddi_have<-n_drug_day + drug
  #for have at least one DDI in one day;
  ddi_have[ddi_have < 101] <-0
  ddi_have_time <-ddi_have[, 1:time]
  #Number of DDI (maximum) within the assessment period;
  ddi_have_time<-as.data.frame(t(ddi_have_time ))
  colMax <- function(data) apply(data, 2, max)
  ddi_num<-colMax(ddi_have_time)
  ddi_num<-as.matrix( ddi_num)
  mean(  ddi_num)-100 #minus "100" because of previous step.
  }



