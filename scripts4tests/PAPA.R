

## PAPA_K1.sav Play and Peer Relationships
## PAPA_K2.sav Sleep Behaviors (K_2_1..) Regulation/Habits (K_2_4..) 
## PAPA_K3.sav ADHD
## PAPA_K4.sav ODD/CD
## PAPA_K5.sav Anxiety

#### not included yet ####
## PAPA_K6.sav Rituals and Repetitions (compulsion, OCD)
## PAPA_K7.sav Depression
## PAPA_K8.sav Tics
## PAPA_K9.sav/PAPA_K10.sav Life Events?



get_PAPA = function(){

  ################################ SLEEP #########################################
  SL = data.table(read_sav("savs/PAPA/PAPA_K2.sav"))
  
  old_names = c("K2_2_1_1_1", "K2_2_1_1_2", "K2_2_1_1_3","K2_2_1_2_1",
                "K2_2_1_3_1", "K2_2_1_3_2_1", "K2_2_1_3_2_2", "K2_2_1_3_2_3", "K2_2_1_3_2_4",
                "K2_2_1_4_1","K2_2_1_5_1","K2_2_1_6_1","K2_2_1_6_2",
                "K2_2_1_8_1","K2_2_1_8_2","K2_2_1_10_1","K2_2_1_11_1",
                "K2_2_1_12_1","K2_2_1_13_1","K2_2_1_14_1","K2_2_1_16_1")
  new_names = c("got_to_bed","get_up","hours_per_night","resists_sleeping",
                "bedtime_ritual","bedtime_ritual.reading","bedtime_ritual.history","bedtime_ritual.singing","bedtime_ritual.other",
                "leaves_bed","wakes_at_night","time_to_fall_asleep","difficulties_sleeping",
                "self_sleeper_day","self_sleeper_evening","restless_sleep","not_rested_after_sleep",
                "Hypersomnia","sleeps_during_day","nightmares","somnabulism")
  new_names = paste("PAPA.sleep.",new_names,sep = "")
  setnames(SL,old_names,new_names)
  
  SL = SL[,c(names(SL)[1:2],new_names),with = F]

  d = SL$PAPA.sleep.hours_per_night
  d = gsub("[ a-z]","",d)
  d = gsub(",",".",d)
  d[d == "??"] = NA
  d[d == "11-12"]= 11.5
  d[d == "10-11"]= 10.5
  d[d == "1015"]= 10.15
  for(k in  grep("-",d))  d[k] = mean(as.numeric(unlist(strsplit(d[k],"-"))))
  
  d = as.numeric(d)
  SL$PAPA.sleep.hours_per_night = d
  
  t = SL$PAPA.sleep.time_to_fall_asleep
  t = gsub("[ a-z]","",t)
  t = gsub(",",".",t)
  for(k in  grep("-",t))  t[k] = mean(as.numeric(unlist(strsplit(t[k],"-"))))
  t = as.numeric(t)
  t = SL$PAPA.sleep.time_to_fall_asleep
  SL$PAPA.sleep.time_to_fall_asleep = t
  
  PAPA = SL
  
  ################################ ADHD #########################################
  AD = data.table(read_sav("savs/PAPA/PAPA_K3.sav"))
  
  AD[is.na(AD$K33_10_2), K33_10_2 := 0]
  #AD$K33_10_1 = 2*(AD$K33_10_1 >= 2 | AD$K33_10_2 >= 2) # from SPSS syntax file "Diagnosegrupper PAPA_konkl juni2012"
  AD$K33_10_1 = as.numeric(cut(AD$K33_10_2 + AD$K33_10_1,breaks = c(-1,1,3.5,6.5))) # modified version that preserves variance
  AD$K33_10_1[AD$K33_10_1 < 2] = 0
  
  items2dims = list(Hyp = c(1,3,4,5,7,8),
                    Imp = c(21,22,23),
                    ATT = c(10:18))
  old_names = paste(paste("K33_",unlist(items2dims),sep = ""),"_1",sep = "")
  new_names = paste("PAPA.ADHD.rating.",names(unlist(items2dims)),sep = "")
  setnames(AD,old_names,new_names)
  old_namesi = paste(paste("K33_",unlist(items2dims),sep = ""),"_2",sep = "")
  new_namesi = paste("PAPA.ADHD.intensity.",names(unlist(items2dims)),sep = "")
  old_namesi = gsub("K33_1_2","K33_1_3",old_namesi)
  old_namesi = gsub("K33_3_2","K33_3_3",old_namesi)
  old_namesi = gsub("K33_10_2","K33_10_4",old_namesi)
  old_namesi = gsub("K33_22_2","K33_22_3",old_namesi)
  
  setnames(AD,old_namesi,new_namesi)
  
  AD = AD[,c(names(AD)[1:2],new_names,new_namesi),with = F]
  
  for(v in new_names) AD[[v]][which(AD[[v]] > 0)] = AD[[v]][which(AD[[v]] > 0)]-1
  
  AD$PAPA.ADHD.Hyperactivity.sum.SCORE = make_sum_scores(AD[,grep("ADHD.rating.Hyp",names(AD)),with = F])
  AD$PAPA.ADHD.Impulsivity.sum.SCORE = make_sum_scores(AD[,grep("ADHD.rating.Imp",names(AD)),with = F])
  AD$PAPA.ADHD.HypImp.sum.SCORE = make_sum_scores(AD[,c(grep("ADHD.rating.Imp",names(AD)),grep("ADHD.rating.Hyp",names(AD))),with = F])
  AD$PAPA.ADHD.Inattention.sum.SCORE = make_sum_scores(AD[,grep("ADHD.rating.ATT",names(AD)),with = F])
  AD$PAPA.ADHD.sum.SCORE = make_sum_scores(AD[,grep("ADHD.rating",names(AD)),with = F])
  
  AD$PAPA.ADHD.Hyperactivity.symCOUNT = make_sum_scores(AD[,grep("ADHD.rating.Hyp",names(AD)),with = F]>0)
  AD$PAPA.ADHD.Impulsivity.symCOUNT = make_sum_scores(AD[,grep("ADHD.rating.Imp",names(AD)),with = F])
  AD$PAPA.ADHD.HypImp.symCOUNT = make_sum_scores(AD[,c(grep("ADHD.rating.Imp",names(AD)),grep("ADHD.rating.Hyp",names(AD))),with = F]>0)
  AD$PAPA.ADHD.Inattention.symCOUNT = make_sum_scores(AD[,grep("ADHD.rating.ATT",names(AD)),with = F]>0)
  AD$PAPA.ADHD.symCOUNT = make_sum_scores(AD[,grep("ADHD.rating",names(AD)),with = F]>0)  
  
   ######################## Impairment score ########################
  ADHDdata = read_sav("savs/PAPA/PAPA_K3.sav")
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
  
  AD$Impair_ADHD_PAPA = Impair_ADHD_PAPA
  AD$imprmt_scoreADHD = imprmt_scoreADHD
  
  ######################## Duration of symptoms ########################
  symptAgeVars = c("K33_2_5", "K33_6_5", "K33_9_4", "K33_19_4", "K33_25_2")
  
  Alder_urolig = as.numeric(gsub("mnd|\\?","",ADHDdata$K33_2_5))
  Alder_ifarta = as.numeric(gsub("[a-z]|å","",ADHDdata$K33_6_5))
  
  ADHDdata$K33_9_4[ADHDdata$K33_9_4 == "18-24"] = "21"
  Alder_brakelek = as.numeric(gsub("[a-z]|å","",ADHDdata$K33_9_4))

  ADHDdata$K33_9_4[ADHDdata$K33_9_4 == "24-30"] = "27"
  Alder_konsvansker = as.numeric(gsub("[a-z]|å","",ADHDdata$K33_19_4))
  
  Alder_imp <- ADHDdata$K33_25_2 <- as.numeric(as.character(ADHDdata$K33_25_2))

  duration_ADHDsympt <- ifelse(Alder_urolig<36 | Alder_ifarta<36 | Alder_brakelek<36 | 
                                 Alder_konsvansker<36 | Alder_imp<36, 1, 0)
  
  AD$duration_ADHDsympt = duration_ADHDsympt
  ######################## ADHD clinical level ########################
  AD[PAPA.ADHD.Inattention.symCOUNT >=6  & PAPA.ADHD.HypImp.symCOUNT  <6 & Impair_ADHD_PAPA == 1, ADHDclin := 1]
  AD[PAPA.ADHD.Inattention.symCOUNT  <6  & PAPA.ADHD.HypImp.symCOUNT >=6 & Impair_ADHD_PAPA == 1, ADHDclin := 2]
  AD[PAPA.ADHD.Inattention.symCOUNT >=6  & PAPA.ADHD.HypImp.symCOUNT >=6 & Impair_ADHD_PAPA == 1, ADHDclin := 3]
  AD[PAPA.ADHD.Inattention.symCOUNT  <6  & PAPA.ADHD.HypImp.symCOUNT  <6 & Impair_ADHD_PAPA == 0, ADHDclin := 4]
  AD[is.na(ADHDclin), ADHDclin:= 4]
  
  AD[,PAPA.ADHD.subtype := factor(ADHDclin,labels = c('ADHD-IA','ADHD-H', 'ADHD-C', 'No_clinADHD'))]
  
  ######################## ADHD subthreshold without impairment ########################
  AD[ADHDclin >3 & PAPA.ADHD.Inattention.symCOUNT >=6 & PAPA.ADHD.HypImp.symCOUNT  <6, ADHDsub_noImp := 1]
  AD[ADHDclin >3 & PAPA.ADHD.Inattention.symCOUNT  <6 & PAPA.ADHD.HypImp.symCOUNT >=6, ADHDsub_noImp := 2]
  AD[ADHDclin >3 & PAPA.ADHD.Inattention.symCOUNT >=6 & PAPA.ADHD.HypImp.symCOUNT >=6, ADHDsub_noImp := 3]
  AD[ADHDclin >3 & PAPA.ADHD.Inattention.symCOUNT  <6 & PAPA.ADHD.HypImp.symCOUNT  <6, ADHDsub_noImp := 4]
  AD[is.na(ADHDsub_noImp), ADHDsub_noImp := 5]

  AD[,PAPA.ADHD.subthresh_imp_types := factor(ADHDsub_noImp,labels = c('subimpADHD_IA','subimpADHD_H', 'subimpADHD_C', 'NoADHD', "clinADHD"))]
  

  ######################## ADHD subthreshold with impairment ########################
  AD[ADHDclin >3 & PAPA.ADHD.Inattention.symCOUNT %in% 3:5 & PAPA.ADHD.HypImp.symCOUNT <3 & Impair_ADHD_PAPA == 1, ADHDsub_wImp := 1]
  AD[ADHDclin >3 & PAPA.ADHD.Inattention.symCOUNT <3 & PAPA.ADHD.HypImp.symCOUNT %in% 3:5 & Impair_ADHD_PAPA == 1, ADHDsub_wImp := 2]
  AD[ADHDclin >3 & PAPA.ADHD.Inattention.symCOUNT %in% 3:5 & PAPA.ADHD.HypImp.symCOUNT %in% 3:5 & Impair_ADHD_PAPA == 1, ADHDsub_wImp := 3]
  AD[ADHDclin >3 & PAPA.ADHD.Inattention.symCOUNT <3 & PAPA.ADHD.HypImp.symCOUNT <3, ADHDsub_wImp := 4]
  AD[is.na(ADHDsub_wImp), ADHDsub_wImp := 5]
  
  AD[,PAPA.ADHD.subthresh_no_imp_types := factor(ADHDsub_wImp,labels = c('subnoimpADHD_IA','subnoimpADHD_H', 'subnoimpADHD_C', 'NoADHD', "clinADHD"))]

  
  ######################## Combined variable for subthreshold ADHD ########################
  AD[ADHDsub_noImp==1 | ADHDsub_wImp==1, ADHDsubthr := 1]
  AD[ADHDsub_noImp==2 | ADHDsub_wImp==2, ADHDsubthr := 2]
  AD[ADHDsub_noImp==3 | ADHDsub_wImp==3, ADHDsubthr := 3]
  AD[is.na(ADHDsub_noImp), ADHDsubthr := 4]
  
  AD[ADHDclin==3, ADHD_Group := 1]
  AD[ADHDclin==2, ADHD_Group := 2]
  AD[ADHDclin==1, ADHD_Group := 3]
  AD[ADHDsubthr==3, ADHD_Group := 4]
  AD[ADHDsubthr==2, ADHD_Group := 5]
  AD[ADHDsubthr==1, ADHD_Group := 6]
  AD[is.na(ADHD_Group), ADHD_Group := 7]
  
  AD[,PAPA.ADHD.Group := factor(ADHD_Group,labels=c('ADHD-C_clin','ADHD-H_clin', 'ADHD-IA_clin', 
                                                   'ADHD-C_sub', 'ADHD-H_sub', 'ADHD-IA_sub', 'No_ADHD'))]
  
  setnames(AD,c("imprmt_scoreADHD","duration_ADHDsympt"),c("PAPA.ADHD.impairement.SCORE","PAPA.ADHD.symptom.dur"))
  
  AD = AD[,c(1:2,grep("PAPA.ADHD",names(AD))),with = F]

  AD$PREG_ID_299 = as.numeric(AD$PREG_ID_299)
  AD$BARN_NR = as.numeric(AD$BARN_NR)
  PAPA = merge(PAPA,AD,by = c("PREG_ID_299","BARN_NR"))
   
  ################################ BEHAVIOR ######################################
  BH = data.table(read_sav("savs/PAPA/PAPA_K4.sav"))
  #BH$K44_14_1 = 2*(BH$K44_14_1 > = 2 | BH$K44_15_1 >= 2) #(Collaps "erter" and "mobber" into variable K44_14).
  #BH$K44_19_1 = 2*(BH$K44_19_1 > = 2 | BH$K44_20_1 >= 2) #(Collaps "sloss" and "angrep" into variable K44_19).
  BH$K44_14_1 = as.numeric(cut(BH$K44_15_1 + BH$K44_14_1,breaks = c(-1,1,3.5,6.5)))
  BH$K44_14_1[BH$K44_14_1<2] = 0
  BH$K44_19_1 = as.numeric(cut(BH$K44_20_1 + BH$K44_19_1,breaks = c(-1,1,3.5,6.5)))
  BH$K44_19_1[BH$K44_19_1<2] = 0
  BH$K44_13_1[BH$K44_13_1 == 9] = NA # 9 was for "snakker ikke" in a question about lying
  
  items2dims = list(ODD = c(3,4,6,8,9,10,11,12),
                    CD = c(13,14,16,17,18,19,21,22))
  
  old_names = paste(paste("K44_",unlist(items2dims),sep = ""),"_1",sep = "")
  new_names = paste("PAPA.BH.rating.",names(unlist(items2dims)),sep = "")
  setnames(BH,old_names,new_names)
  
  for(v in new_names) BH[[v]][which(BH[[v]] > 0)] = BH[[v]][which(BH[[v]] > 0)]-1
  
  BH = BH[,c(names(BH)[1:2],new_names),with = F]
  BH$PAPA.BH.ODD.sum.SCORE = make_sum_scores(BH[,grep("BH.rating.ODD",names(BH)),with = F])
  BH$PAPA.BH.CD.sum.SCORE = make_sum_scores(BH[,grep("BH.rating.CD",names(BH)),with = F])
  
  BH$PREG_ID_299 = as.numeric(BH$PREG_ID_299)
  BH$BARN_NR = as.numeric(BH$BARN_NR)
  PAPA = merge(PAPA,BH,by = c("PREG_ID_299","BARN_NR"))
  ################################ ANXIETY ######################################
  AX = data.table(read_sav("savs/PAPA/PAPA_K5.sav"))

    items2dims = list(PHOB = 2:8,
                    SOC = 9:11,
                    SEP = c(13:14,16:19),
                    GEN = 22:25)
  item2dimsAUTSYM = 16 # ANXIOUS AUTONOMIC SYMPTOMS 
  
  old_names = c(paste(paste("K55_",unlist(items2dims),sep = ""),"_1",sep = ""),
                paste("K55_21_",item2dimsAUTSYM,sep = ""))
  new_names = c(paste("PAPA.ANX.rating.",names(unlist(items2dims)),sep = ""),
                paste("PAPA.ANX.rating.AUTSYM",item2dimsAUTSYM,sep = ""))
  
  setnames(AX,old_names,new_names)
  
  for(v in new_names) AX[[v]][which(AX[[v]] > 0)] = AX[[v]][which(AX[[v]] > 0)]-1
  
  AX = AX[,c(names(AX)[1:2],new_names),with = F]
  AX$PAPA.ANX.PHOB.sum.SCORE = make_sum_scores(AX[,grep("ANX.rating.PHOB",names(AX)),with = F])
  AX$PAPA.ANX.SOC.sum.SCORE = make_sum_scores(AX[,grep("ANX.rating.SOC",names(AX)),with = F])
  AX$PAPA.ANX.SEP.sum.SCORE = make_sum_scores(AX[,grep("ANX.rating.SEP",names(AX)),with = F])
  AX$PAPA.ANX.GEN.sum.SCORE = make_sum_scores(AX[,grep("ANX.rating.GEN",names(AX)),with = F])

  AX$PREG_ID_299 = as.numeric(AX$PREG_ID_299)
  AX$BARN_NR = as.numeric(AX$BARN_NR)
  PAPA = merge(PAPA,AX,by = c("PREG_ID_299","BARN_NR"))
  
  return(PAPA)
}


