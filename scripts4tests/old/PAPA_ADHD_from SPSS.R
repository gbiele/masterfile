library(plyr)

get_ADHD_PAPA = function() {
  ADHDdata = read_sav("savs/PAPA/PAPA_K3.sav")
  #*# OLD #*# by Guido
  #ADHDdata$K33_10_1 = 2*(ADHDdata$K33_10_1 >= 2 | ADHDdata$K33_10_2 >= 2) # from SPSS syntax file "Diagnosegrupper PAPA_konkl juni2012"
  ADHDdata$K33_10_1 = as.numeric(cut(ADHDdata$K33_10_2 + ADHDdata$K33_10_1,breaks = c(-1,1,3.5,6.5))) # modified version that preserves variance
  ADHDdata$K33_10_1[ADHDdata$K33_10_1 < 2] = 0
  ADHDdata$K33_10 <- ifelse(ADHDdata$K33_10_1>1 | ADHDdata$K33_10_2 >1, 2, 0)
  HypVars = c("K33_1_1", "K33_3_1", "K33_4_1", "K33_5_1", "K33_7_1", "K33_8_1")
  ImpVars = c("K33_21_1", "K33_22_1", "K33_23_1")
  AttVars = c("K33_10", "K33_11_1", "K33_12_1", "K33_13_1", "K33_14_1", "K33_15_1", "K33_16_1", "K33_17_1", "K33_18_1")
  
  #*# NEW - from SPSS syntax #*# by Ida
  ######################## Count number of symptoms ########################
  HypSympt <- c()
  for(i in 1:nrow(ADHDdata)){
    HypSympt[i] <- length(which(ADHDdata[i,c(HypVars)]>1))
  }
  ImpSympt <- c()
  for(i in 1:nrow(ADHDdata)){
    ImpSympt[i] <- length(which(ADHDdata[i,c(ImpVars)]>1))
  }
  AttSympt <- c()
  for(i in 1:nrow(ADHDdata)){
    AttSympt[i] <- length(which(ADHDdata[i,c(AttVars)]>1))
  }
  HypImpSympt <- c()
  for(i in 1:nrow(ADHDdata)){
    HypImpSympt[i] <- length(which(ADHDdata[i,c(HypVars, ImpVars)]>1))
  }
  # Sum all ADHD symptoms
  ADHDsympt <- rowSums(data.frame(AttSympt,HypImpSympt))
  
  ######################## Impairment score ########################
  impairVars = c("K33_28_1", "K33_28_2", "K33_28_3", "K33_28_4", "K33_28_5", "K33_28_6")
  missing_impair <- c()
  for(i in 1:nrow(ADHDdata)){
    missing_impair[i] <- length(which(is.na(ADHDdata[i,c(impairVars)])))
  }
  litt_impair <-c()
  for(i in 1:nrow(ADHDdata)){
    litt_impair[i] <- length(which(ADHDdata[i,c(impairVars)]==1))
  }
  mye_impair <- c()
  for(i in 1:nrow(ADHDdata)){
    mye_impair[i] <- length(which(ADHDdata[i,c(impairVars)]>1))
  }
  
  # Impairment score: 0= no, 1= yes, missing='NA' (SPSS syntax: missing=9)
  Impair_ADHD_PAPA <- rep(0,nrow(ADHDdata))
  Impair_ADHD_PAPA = ifelse(missing_impair>=6, Impair_ADHD_PAPA <- NA, 
         ifelse(missing_impair<6 & (litt_impair>=2 | mye_impair >0), Impair_ADHD_PAPA <- 1, 
                Impair_ADHD_PAPA <- 0))
  
  ### NB. Ikke lik som SPSS-syntaksen (flere NA's her...)
  # Skal NA telles som null der resten av observasjonene ikke er missing?
  imprmt_scoreADHD = rowSums(ADHDdata[,c(impairVars)], na.rm=F)
  
  ######################## Duration of symptoms ########################
  symptAgeVars = c("K33_2_5", "K33_6_5", "K33_9_4", "K33_19_4", "K33_25_2")
  
  ADHDdata$K33_2_5[ADHDdata$K33_2_5=="12 mnd            "] <- '12                '
  ADHDdata$K33_2_5[ADHDdata$K33_2_5=="?                 "] <- NA
  ADHDdata$K33_6_5[ADHDdata$K33_6_5=="10 mån           "] <- '10                '
  ADHDdata$K33_6_5[ADHDdata$K33_6_5=="20mnd             "] <- '20                '
  ADHDdata$K33_9_4[ADHDdata$K33_9_4=="10 mån           "] <- '10                '
  ADHDdata$K33_9_4[ADHDdata$K33_9_4=="18-24             "] <- '21                '
  ADHDdata$K33_9_4[ADHDdata$K33_9_4=="20mnd             "] <- '20                '
  ADHDdata$K33_19_4[ADHDdata$K33_19_4=="24-30             "] <- '27                '
  ADHDdata$K33_19_4[ADHDdata$K33_19_4=="24mnd             "] <- '24                '
  ADHDdata$K33_19_4[ADHDdata$K33_19_4=="vet ik            "] <- NA
  
  Alder_urolig <- ADHDdata$K33_2_5 <- as.numeric(as.character(ADHDdata$K33_2_5))
  Alder_ifarta <- ADHDdata$K33_6_5 <- as.numeric(as.character(ADHDdata$K33_6_5))
  Alder_brakelek <- ADHDdata$K33_9_4 <- as.numeric(as.character(ADHDdata$K33_9_4))
  Alder_konsvansker <- ADHDdata$K33_19_4 <- as.numeric(as.character(ADHDdata$K33_19_4))
  Alder_imp <- ADHDdata$K33_25_2 <- as.numeric(as.character(ADHDdata$K33_25_2))
  
  duration_ADHDsympt <- ifelse(Alder_urolig<36 | Alder_ifarta<36 | Alder_brakelek<36 | 
                                 Alder_konsvansker<36 | Alder_imp<36, 1, 0)
  
  ######################## ADHD clinical level ########################
  ADHDclin <- 
    ifelse(AttSympt >=6 & HypImpSympt<6 & Impair_ADHD_PAPA==1, 1,
     ifelse(AttSympt <6 & HypImpSympt >=6 & Impair_ADHD_PAPA==1, 2, 
      ifelse(AttSympt >=6 & HypImpSympt >=6 & Impair_ADHD_PAPA==1, 3,
       ifelse(AttSympt <6 & HypImpSympt <6 & Impair_ADHD_PAPA==0, 4, 4))))
  ADHDclin[is.na(ADHDclin)] <- 4
  #ADHDclin <- factor(ADHDclin, levels=c('1','2','3','4'), labels=c('ADHD-IA','ADHD-H', 'ADHD-C', 'No_clinADHD'))
  #count(ADHDclin)

  ######################## ADHD sub types ########################
  ADHDsubtype <- 
    ifelse(AttSympt >=6 & HypImpSympt<6 & Impair_ADHD_PAPA==1, 1,
           ifelse(AttSympt <6 & HypImpSympt >=6 & Impair_ADHD_PAPA==1, 2, 
                  ifelse(AttSympt >=6 & HypImpSympt >=6 & Impair_ADHD_PAPA==1, 3,
                         ifelse(AttSympt <6 & HypImpSympt <6 & Impair_ADHD_PAPA==0, 4, 4))))
  ADHDsubtype[is.na(ADHDsubtype)] <- 4
  ADHDsubtype <- factor(ADHDsubtype, levels=c('1','2','3','4'), labels=c('ADHD-IA','ADHD-H', 'ADHD-C', 'No_clinADHD'))
  #count(ADHDclin)
  
    
  ######################## ADHD subthreshold without impairment ########################
  ADHDsub_noImp <- 
    ifelse(ADHDclin>3 & AttSympt>=6 & HypImpSympt<6, 1,
     ifelse(ADHDclin>3 & AttSympt<6 & HypImpSympt>=6, 2, 
      ifelse(ADHDclin>3 & AttSympt>=6 & HypImpSympt>=6, 3,
       ifelse(ADHDclin>3 & AttSympt<6 & HypImpSympt<6, 4, 5))))
  #ADHDsub_noImp <- factor(ADHDsub_noImp, levels=c('1','2','3','4'), labels=c('SubADHD-IA','SubADHD-H', 'SubADHD-C', 'No_SubADHD'))
  
  ######################## ADHD subthreshold with impairment ########################
  ADHDsub_wImp <- 
    ifelse(ADHDclin>3 & AttSympt %in% c(3:5) & HypImpSympt<3 & Impair_ADHD_PAPA==1, 1,
     ifelse(ADHDclin>3 & AttSympt <3 & HypImpSympt  %in% c(3:5) & Impair_ADHD_PAPA==1, 2, 
      ifelse(ADHDclin>3 & AttSympt <6 & HypImpSympt  %in% c(3:5) & Impair_ADHD_PAPA==1, 3,
       ifelse(ADHDclin>3 & AttSympt<3 & HypImpSympt<3, 4, 5))))
  ADHDsub_wImp[is.na(ADHDsub_wImp)] <- 5
  #ADHDsub_wImp <- factor(ADHDsub_wImp, levels=c('1','2','3','4'), labels=c('SubADHD-IA','SubADHD-H', 'SubADHD-C', 'No_SubADHD'))
  #count(ADHDsub_wImp)
  
  ######################## Combined variable for subthreshold ADHD ########################
  ADHDsubthr <- 
    ifelse(ADHDsub_noImp==1 | ADHDsub_wImp==1, 1, 
     ifelse(ADHDsub_noImp==2 | ADHDsub_wImp==2, 2,
      ifelse(ADHDsub_noImp==3 | ADHDsub_wImp==3, 3, 4)))
  count(ADHDsubthr)   
  
  ADHD_Group <- 
    ifelse(ADHDclin==3, 1, 
     ifelse(ADHDclin==2, 2,
      ifelse(ADHDclin==1, 3,   
       ifelse(ADHDsubthr==3, 4,  
        ifelse(ADHDsubthr==2, 5,  
         ifelse(ADHDsubthr==1, 6, 7))))))
  count(ADHD_Group)  
  ADHD_Group <- factor(ADHD_Group, levels=c('1','2','3','4','5','6','7'), 
                       labels=c('ADHD-C_clin','ADHD-H_clin', 'ADHD-IA_clin', 
                                'ADHD-C_sub', 'ADHD-H_sub', 'ADHD-IA_sub', 'No_ADHD'))
  
  
  ###############
  ADHDdata[,c("Hypsympt", "Impsympt","AttSympt",
              "Hyp_imp_sympt", "ADHDsympt", "ADHD_klin", "ADHD_Group","ADHDsubtype","ADHD_impairement_score")] <- data.frame(HypSympt, ImpSympt, AttSympt,
                      HypImpSympt, ADHDsympt, ADHDclin, ADHD_Group,ADHDsubtype,imprmt_scoreADHD)
  
  ADHDoutcomes <- ADHDdata[,c("PREG_ID_299", "BARN_NR", "Hypsympt", "Impsympt","AttSympt",
                              "Hyp_imp_sympt", "ADHDsympt", "ADHD_klin", "ADHD_Group")] 
  return(ADHDoutcomes)
}
# write.table(ADHDoutcomes, "ADHD_outcomevars_PAPA.txt", sep="\t", dec=".")



