

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
  new_names = paste("SLEEP.",new_names,sep = "")
  setnames(SL,old_names,new_names)
  
  SL = SL[,c(names(SL)[1:2],new_names),with = F]

  d = SL$SLEEP.hours_per_night
  d = gsub("[ a-z]","",d)
  d = gsub(",",".",d)
  d[d == "??"] = NA
  d[d == "11-12"]= 11.5
  d[d == "10-11"]= 10.5
  d[d == "1015"]= 10.15
  for(k in  grep("-",d))  d[k] = mean(as.numeric(unlist(strsplit(d[k],"-"))))
  
  d = as.numeric(d)
  SL$SLEEP.hours_per_night = d
  
  t = SL$SLEEP.time_to_fall_asleep
  t = gsub("[ a-z]","",t)
  t = gsub(",",".",t)
  for(k in  grep("-",t))  t[k] = mean(as.numeric(unlist(strsplit(t[k],"-"))))
  t = as.numeric(t)
  t = SL$SLEEP.time_to_fall_asleep
  SL$SLEEP.time_to_fall_asleep = t
  
   rm(d,k,old_names,new_names,t)
  ################################ ADHD #########################################
  AD = data.table(read_sav("savs/PAPA/PAPA_K3.sav"))
  
  #*(Collaps "selvstendig" and "voksenstyrt" activity into variable K33_10).
  AD[K33_10_1 >= 2 | K33_10_2 >= 2, K33_10 := 2 ]
  
  hyp_cols = paste("K33",c(1,3:5,7,8),"1",sep = "_")
  AD[,ADHD.HYP.SYMPTOM.COUNT := sum(.SD > 1,na.rm = T),by = 1:nrow(AD),.SDcols = hyp_cols]
  imp_cols = paste("K33",c(21:23),"1",sep = "_")
  AD[,ADHD.IMP.SYMPTOM.COUNT := sum(.SD > 1,na.rm = T),by = 1:nrow(AD),.SDcols = imp_cols]
  att_cols = c("K33_10",paste("K33",c(11:18),"1",sep = "_"))
  AD[,ADHD.ATT.SYMPTOM.COUNT := sum(.SD > 1,na.rm = T),by = 1:nrow(AD),.SDcols = att_cols]
  AD[,ADHD.HYPIMP.SYMPTOM.COUNT :=  ADHD.HYP.SYMPTOM.COUNT + ADHD.IMP.SYMPTOM.COUNT]
  AD[,ADHD.SYMPTOM.COUNT :=  ADHD.HYP.SYMPTOM.COUNT + ADHD.IMP.SYMPTOM.COUNT + ADHD.ATT.SYMPTOM.COUNT]
  
  ######################## Impairment score ########################
  imp_cols = paste("K33_28",1:6,sep = "_")
  AD[,ADHD.IMPAIR.MISSING := sum(.SD,na.rm = T),by = 1:nrow(AD),.SDcols = imp_cols]
  AD[,ADHD.IMPAIRweak := sum(.SD == 1,na.rm = T),by = 1:nrow(AD),.SDcols = imp_cols]
  AD[,ADHD.IMPAIRstrong := sum(.SD > 1,na.rm = T),by = 1:nrow(AD),.SDcols = imp_cols]
  
  AD[,ADHD.IMPAIR := 0]
  AD[ADHD.IMPAIR.MISSING >= 6, ADHD.IMPAIR := NA]
  AD[ADHD.IMPAIR.MISSING < 6 & 
       (ADHD.IMPAIRweak >= 2 | ADHD.IMPAIRstrong > 0) , ADHD.IMPAIR := 1]
  AD[,ADHD.IMPAIR.CAT := factor(ADHD.IMPAIR,labels = c("absent","present"))]
  
  AD[,ADHD.IMPAIR.SCORE := sum(.SD,na.rm = T),by = 1:nrow(AD),.SDcols = imp_cols]
  
  ######################## ADHD sub groups ########################
  my_labels = c("ADHD_IA" = 1,"ADHD_H" = 2,"ADHD_C" = 3,"no_ADHD" = 4)
  AD[ADHD.IMPAIR.CAT == "present" & ADHD.ATT.SYMPTOM.COUNT >= 6 & ADHD.HYPIMP.SYMPTOM.COUNT < 6, ADHD.SUBGROUP := 1]
  AD[ADHD.IMPAIR.CAT == "present" & ADHD.ATT.SYMPTOM.COUNT < 6 & ADHD.HYPIMP.SYMPTOM.COUNT >= 6, ADHD.SUBGROUP := 2]
  AD[ADHD.IMPAIR.CAT == "present" & ADHD.ATT.SYMPTOM.COUNT >= 6 & ADHD.HYPIMP.SYMPTOM.COUNT >= 6, ADHD.SUBGROUP := 3]
  AD[ADHD.IMPAIR.CAT == "absent" | (ADHD.ATT.SYMPTOM.COUNT < 6 & ADHD.HYPIMP.SYMPTOM.COUNT < 6), ADHD.SUBGROUP := 4]
  AD[,ADHD.SUBGROUP := labelled(ADHD.SUBGROUP,labels = my_labels)]
  
  ######################## ADHD subthreshold without impairment ########################
  my_labels = c("ADHD_subthr_woi_IA" = 1,"ADHD_subthr_woi_H" = 2,"ADHD_subthr_woi_C" = 3,"no_ADHD_subthr_woi" = 4)
  AD[ADHD.SUBGROUP == 4 & ADHD.ATT.SYMPTOM.COUNT >= 6 & ADHD.HYPIMP.SYMPTOM.COUNT < 6, ADHD.SUBGROUPsubthr_woi := 1]
  AD[ADHD.SUBGROUP == 4 & ADHD.ATT.SYMPTOM.COUNT < 6 & ADHD.HYPIMP.SYMPTOM.COUNT >= 6, ADHD.SUBGROUPsubthr_woi := 2]
  AD[ADHD.SUBGROUP == 4 & ADHD.ATT.SYMPTOM.COUNT >= 6 & ADHD.HYPIMP.SYMPTOM.COUNT >= 6, ADHD.SUBGROUPsubthr_woi := 3]
  AD[ADHD.SUBGROUP == 4 & ADHD.ATT.SYMPTOM.COUNT < 6 & ADHD.HYPIMP.SYMPTOM.COUNT < 6, ADHD.SUBGROUPsubthr_woi := 4]
  AD[,ADHD.SUBGROUPsubthr_woi := labelled(ADHD.SUBGROUPsubthr_woi,labels = my_labels)]
  
  ######################## ADHD subthreshold with impairment ########################
  my_labels = c("ADHD_subthr_wi_IA" = 1,"ADHD_subthr_wi_H" = 2,"ADHD_subthr_wi_C" = 3,"no_ADHD_subthr_wi" = 4)
  AD[ADHD.SUBGROUP == 4 & ADHD.ATT.SYMPTOM.COUNT %in% 3:5 & ADHD.HYPIMP.SYMPTOM.COUNT < 3 & ADHD.IMPAIR.CAT == "present", ADHD.SUBGROUPsubthr_wi := 1]
  AD[ADHD.SUBGROUP == 4 & ADHD.ATT.SYMPTOM.COUNT < 3 & ADHD.HYPIMP.SYMPTOM.COUNT %in% 3:5 & ADHD.IMPAIR.CAT == "present", ADHD.SUBGROUPsubthr_wi := 2]
  AD[ADHD.SUBGROUP == 4 & ADHD.ATT.SYMPTOM.COUNT %in% 3:5 & ADHD.HYPIMP.SYMPTOM.COUNT %in% 3:5 & ADHD.IMPAIR.CAT == "present", ADHD.SUBGROUPsubthr_wi := 3]
  AD[ADHD.SUBGROUP == 4 & ADHD.ATT.SYMPTOM.COUNT < 3 & ADHD.HYPIMP.SYMPTOM.COUNT < 3, ADHD.SUBGROUPsubthr_wi := 4]
  AD[,ADHD.SUBGROUPsubthr_wi := labelled(ADHD.SUBGROUPsubthr_wi,labels = my_labels)]
  
  ######################## Combined variable for subthreshold ADHD ########################
  my_labels = c("ADHD_subthr_IA" = 1,"ADHD_subthr_H" = 2,"ADHD_subthr_C" = 3,"no_ADHD_subthr" = 4)
  AD[ADHD.SUBGROUPsubthr_woi == 1 | ADHD.SUBGROUPsubthr_wi == 1, ADHD.SUBGROUPsubthr := 1]
  AD[is.na(ADHD.SUBGROUPsubthr) & ADHD.SUBGROUPsubthr_woi == 2 | ADHD.SUBGROUPsubthr_wi == 2, ADHD.SUBGROUPsubthr := 2]
  AD[is.na(ADHD.SUBGROUPsubthr) & ADHD.SUBGROUPsubthr_woi == 3 | ADHD.SUBGROUPsubthr_wi == 3, ADHD.SUBGROUPsubthr := 3]
  AD[is.na(ADHD.SUBGROUPsubthr) , ADHD.SUBGROUPsubthr := 4]
  AD[,ADHD.SUBGROUPsubthr := labelled(ADHD.SUBGROUPsubthr,labels = my_labels)]
  
  my_labels = c("ADHD_c_clin" = 1,"ADHD_H_clin" = 2,"ADHD_IA_clin" = 3,"ADHD_C_subclin" = 4,"ADHD_H_subclin" = 5,"ADHD_IA_subclin" = 6,"no_ADHD" = 7)
  AD[ADHD.SUBGROUP < 4, ADHD.GROUP := abs(ADHD.SUBGROUP-4)]
  AD[ADHD.SUBGROUP > 3 & ADHD.SUBGROUPsubthr < 4, ADHD.GROUP := abs(ADHD.SUBGROUPsubthr-4)+3]
  AD[is.na(ADHD.GROUP), ADHD.GROUP := 7]
  AD[,ADHD.GROUP := labelled(ADHD.GROUP,labels = my_labels)]
  
  ############################ ADHD CATEGORY #############################
  AD[ADHD.GROUP <= 3 ,ADHD.CATEGORY := 1]
  AD[ADHD.GROUP %in% 4:6 ,ADHD.CATEGORY := 2]
  AD[is.na(ADHD.CATEGORY) ,ADHD.CATEGORY := 3]
  my_labels = c("ADHD_clin" = 1, "ADHD_subclin" = 2, "no_ADHD" = 3)
  AD[,ADHD.CATEGORY := labelled(ADHD.CATEGORY,labels = my_labels)]
  
  ############################ ADHD sum scores #############################
  items2dims = list(HYP = c(1,3,4,5,7,8),
                    IMP = c(21,22,23),
                    ATT = c(10:18))
  old_names = paste(paste("K33_",unlist(items2dims),sep = ""),"_1",sep = "")
  new_names = paste("ADHD.SYMPTOM.SCORE.",names(unlist(items2dims)),sep = "")
  setnames(AD,old_names,new_names)
  AD = make_sum_scores(AD,names(AD)[grep("SYMPTOM.SCORE.HYP",names(AD))],"ADHD.SYMPTOM.SCORE.HYP")
  AD = make_sum_scores(AD,names(AD)[grep("SYMPTOM.SCORE.IMP",names(AD))],"ADHD.SYMPTOM.SCORE.IMP")
  AD = make_sum_scores(AD,names(AD)[grep("SYMPTOM.SCORE.ATT",names(AD))],"ADHD.SYMPTOM.SCORE.ATT")
  SDcols = c("ADHD.SYMPTOM.SCORE.HYP" , "ADHD.SYMPTOM.SCORE.IMP" , "ADHD.SYMPTOM.SCORE.ATT")
  AD[,ADHD.SYMPTOM.SCORE := sum(.SD),by = 1:nrow(AD),.SDcols = SDcols]
  
  AD[,PREG_ID_299 := as.numeric(PREG_ID_299)]
  AD[,BARN_NR := as.numeric(BARN_NR)]
  
  rm(att_cols,hyp_cols,imp_cols,items2dims,new_names,old_names)
  
  #######################################################################
  ############################### BH: ODD ###############################
  #######################################################################
  
  BH = data.table(read_sav("savs/PAPA/PAPA_K4.sav"))
  #BH$K44_14_1 = 2*(BH$K44_14_1 > = 2 | BH$K44_15_1 >= 2) #(Collaps "erter" and "mobber" into variable K44_14).
  #BH$K44_19_1 = 2*(BH$K44_19_1 > = 2 | BH$K44_20_1 >= 2) #(Collaps "sloss" and "angrep" into variable K44_19).
  BH$K44_14_1 = as.numeric(cut(BH$K44_15_1 + BH$K44_14_1,breaks = c(-1,1,3.5,6.5)))
  BH$K44_14_1[BH$K44_14_1<2] = 0
  BH$K44_19_1 = as.numeric(cut(BH$K44_20_1 + BH$K44_19_1,breaks = c(-1,1,3.5,6.5)))
  BH$K44_19_1[BH$K44_19_1<2] = 0
  BH$K44_13_1[BH$K44_13_1 == 9] = NA # 9 was for "snakker ikke" in a question about lying
  
  
  # symptom duration
  freq_vars = sort(c(paste("K44",c(3,4,6,9,12),"2",sep = "_"),paste("K44",c(10,11),"6",sep = "_"),"K44_8_4"))
  for (v in freq_vars){
    BH[,tmp := char2num(get(v)),by = 1:nrow(BH)]
    BH[tmp > 90,tmp := 90]
    BH[tmp < 1,tmp := NA]
    eval(parse(text=paste0("BH[,",v,":=NULL]")))
    setnames(BH,"tmp",v)
  }
  
  
  # symptom present
  ## Symptom til stede defineres som skåre 2 + høy frekvens.
  BH[K44_3_1 > 1 & K44_3_2 >= 90,K44_3_1F := K44_3_1]
  BH[K44_4_1 > 1 & K44_4_2 >= 48,K44_4_1F := K44_4_1]
  BH[K44_6_1 > 1 & K44_6_2 >= 85,K44_6_1F := K44_6_1]
  BH[K44_8_1 > 1 & K44_8_4 >= 44,K44_8_1F := K44_8_1]
  BH[K44_9_1 > 1 & K44_9_2 >= 1 ,K44_9_1F := K44_9_1]
  BH[K44_10_1 > 1 & K44_10_6 >= 1,K44_10_1F := K44_10_1]
  BH[K44_11_1 > 1 & K44_11_6 >= 21 ,K44_11_1F := K44_11_1]
  BH[K44_12_1 > 1 & K44_12_2 >= 1,K44_12_1F := K44_12_1]
  
 
  # * Her telles antall ODD-symptomer med hyppighet tatt i betraktning.
  SDcols = names(BH)[intersect(grep("^K44",names(BH)),grep("1F$",names(BH)))]
  BH[,BH.ODD.SYMPTOM.COUNT := sum(.SD > 1,na.rm = T),by = 1:nrow(BH),.SDcols = SDcols]

  # * Her telles antall ODD-symptomer uavhengig av hyppighet av forekomst.
  SDcols =  gsub("_2|_6|_4","_1",freq_vars)
  BH[,BH.ODD.SYMPTwofreq := sum(.SD > 1,na.rm = T),by = 1:nrow(BH),.SDcols = SDcols]
  BH[,BH.ODD.SYMPTwofreq.MISSING := sum(is.na(.SD)),by = 1:nrow(BH),.SDcols = SDcols]
  
 # impairment   
  impair_cols = paste("K44_23A",1:6,sep = "_")
  BH[,BH.ODD.IMPAIR.MISSING := sum(is.na(.SD)),by = 1:nrow(BH), .SDcols = impair_cols]
  BH[,BH.ODD.IMPAIR.WEAK := sum(.SD == 1,na.rm = T),by = 1:nrow(BH), .SDcols = impair_cols]
  BH[,BH.ODD.IMPAIR.STRONG := sum(.SD > 1,na.rm = T),by = 1:nrow(BH), .SDcols = impair_cols]
  
  BH[, BH.ODD.IMPAIR := 0]
  BH[(BH.ODD.IMPAIR.MISSING < 6 & BH.ODD.IMPAIR.WEAK > 2) | BH.ODD.IMPAIR.STRONG > 0, BH.ODD.IMPAIR := 1]
  BH[BH.ODD.IMPAIR.MISSING >= 6, BH.ODD.IMPAIR := NA]

  SDcols = paste("K44_23A",1:6,sep = "_")
  BH[,BH.ODD.IMPAIR.SCORE := sum(.SD,na.rm = T),by = 1:nrow(BH),.SDcols = SDcols]
  BH[,BH.ODD.IMPAIR.SCORE.MISSING := sum(is.na(.SD)),by = 1:nrow(BH),.SDcols = SDcols]
  BH[BH.ODD.IMPAIR.SCORE.MISSING > 2, BH.ODD.IMPAIR.SCORE := NA]

  
  SDcols = paste("K44",c(3,4,6,8,9,10,11,12),"1F",sep = "_")
  BH[,BH.ODD.SYMPTOM.COUNT := sum(.SD > 0,na.rm = T),by = 1:nrow(BH), .SDcols = SDcols]

  BH[, BH.ODD.GROUP := 5]
  BH[BH.ODD.SYMPTOM.COUNT >= 4 & BH.ODD.IMPAIR == 1, BH.ODD.GROUP := 1]
  BH[BH.ODD.GROUP > 1 & 
       ((BH.ODD.SYMPTOM.COUNT > 4 & BH.ODD.IMPAIR == 0) |
       (BH.ODD.SYMPTOM.COUNT %in% 1:3 & BH.ODD.IMPAIR == 1)), BH.ODD.GROUP := 2]
  BH[BH.ODD.GROUP > 2 & BH.ODD.SYMPTOM.COUNT >= 4 & is.na(BH.ODD.IMPAIR), BH.ODD.GROUP := 3]
  BH[BH.ODD.GROUP > 3 & 
       (BH.ODD.SYMPTOM.COUNT <  4 & (BH.ODD.IMPAIR == 0 | is.na(BH.ODD.IMPAIR))) | 
       BH.ODD.SYMPTOM.COUNT == 0, BH.ODD.GROUP := 4]
  BH[BH.ODD.GROUP == 5, BH.ODD.GROUP := NA]
  
  my_labels = c("ODD_clin" = 1, "ODD_subclin" = 2, "ODD_missimp" = 3, "no_ODD" = 4)
  BH[,BH.ODD.GROUP := labelled(BH.ODD.GROUP,labels = my_labels)]
  
  rm(freq_vars,impair_cols,SDcols,v)
  
  ######################################################################
  ############################### BH: CD ###############################
  ######################################################################
  
  # *(Slår sammen "erter" og "mobber" i variabel K44_14).
  BH[K44_14_1 >= 2 | K44_15_1 >= 2, K44_14 := 2]
  # *(Slår sammen "sloss" og "angrep" i variabel K44_19).
  BH[K44_19_1 >= 2 | K44_20_1 >= 2, K44_19 := 2]
  
  BH[,BH.CD.SYMPTOM.COUNT := sum(.SD > 1,na.rm = T),by = 1:nrow(BH),
     .SDcols = c("K44_13_1", "K44_16_1", "K44_17_1", "K44_18_1", "K44_21_1", "K44_22_1", "K44_14")]
  BH[,BH.CD.SYMPT.MISSING := sum(is.na(.SD),na.rm = T),by = 1:nrow(BH),
     .SDcols = c("K44_13_1", "K44_16_1", "K44_17_1", "K44_18_1", "K44_21_1", "K44_22_1", "K44_14")]
  BH[BH.CD.SYMPT.MISSING > 3, BH.CD.SYMPTOM.COUNT := NA]
  
  cd.imp.cols = paste("K44_23B",1:6,sep = "_") 
  BH[,BH.CD.IMPAIR.MISSING := sum(.SD,na.rm = T),by = 1:nrow(BH),.SDcols = cd.imp.cols]
  BH[,BH.CD.IMPAIRweak := sum(.SD == 1,na.rm = T),by = 1:nrow(BH),.SDcols = cd.imp.cols]
  BH[,BH.CD.IMPAIRstrong := sum(.SD > 1,na.rm = T),by = 1:nrow(BH),.SDcols = cd.imp.cols]
 
  BH[,BH.CD.IMPAIR := 0]
  BH[BH.CD.IMPAIR.MISSING >= 6, BH.CD.IMPAIR := NA]
  BH[BH.CD.IMPAIR.MISSING < 6 & 
       (BH.CD.IMPAIRweak >= 2 | BH.CD.IMPAIRstrong > 0) , BH.CD.IMPAIR := 1]
  BH[,BH.CD.IMPAIR.CAT := factor(BH.CD.IMPAIR,labels = c("absent","present"))]
  
  BH[,BH.CD.IMPAIR.SCORE := sum(.SD,na.rm = T),by = 1:nrow(BH),.SDcols = cd.imp.cols]
 
  BH[,BH.CD.GROUP := 4]
  BH[BH.CD.SYMPTOM.COUNT >= 3 & BH.CD.IMPAIR == 1,BH.CD.GROUP := 1]
  BH[BH.CD.GROUP > 1 & 
       (BH.CD.SYMPTOM.COUNT >= 3 & BH.CD.IMPAIR == 0) | 
       (BH.CD.SYMPTOM.COUNT %in% 1:2 & BH.CD.IMPAIR == 1),
     BH.CD.GROUP := 2]
  BH[BH.CD.GROUP > 2 & BH.CD.SYMPTOM.COUNT >= 3 & is.na(BH.CD.IMPAIR), BH.CD.GROUP := 3]
  BH[is.na(BH.CD.SYMPTOM.COUNT) & is.na(BH.CD.IMPAIR), BH.CD.GROUP := NA]
 
  my_labels = c("CD_clin" = 1, "CD_subclin" = 2, "CD_missimp" = 3, "no_CD" = 4)
  BH[,BH.CD.GROUP := labelled(BH.CD.GROUP,labels = my_labels)]
  
  
  rm(cd.imp.cols)
  
  
  BH[,BH.DBD.GROUP := 4]
  BH[BH.ODD.GROUP == 1 | BH.CD.GROUP == 1, BH.DBD.GROUP := 1]
  BH[BH.DBD.GROUP > 1 & (BH.ODD.GROUP == 2 | BH.CD.GROUP == 2), BH.DBD.GROUP := 2]
  BH[BH.DBD.GROUP > 2 & (BH.ODD.GROUP == 3 | BH.CD.GROUP == 3), BH.DBD.GROUP := 3]
  
  my_labels = c("DBD_clin" = 1, "DBD_subclin" = 2, "DBD_missimp" = 3, "no_DBD" = 4)
  BH[,BH.DBD.GROUP := labelled(BH.DBD.GROUP,labels = my_labels)]
  
  
  BH$PREG_ID_299 = as.numeric(BH$PREG_ID_299)
  BH$BARN_NR = as.numeric(BH$BARN_NR)
  
  items2dims = list(ODD = c(3,4,6,8,9,10,11,12),
                    CD = c(13,14,16,17,18,19,21,22))
  
  old_names = paste(paste("K44_",unlist(items2dims),sep = ""),"_1",sep = "")
  new_names = paste("BH.rating.",names(unlist(items2dims)),sep = "")
  setnames(BH,old_names,new_names)
  for(v in new_names) {
    eval(parse(text = 
                 paste0("BH[",v," > 0, ",v," := ",v,"-1,by = list(PREG_ID_299,BARN_NR)]")
               ))
  } 
  
  BH = make_sum_scores(BH,names(BH)[grep("BH.rating.ODD",names(BH))],"BH.ODD.sum.SCORE")
  BH = make_sum_scores(BH,names(BH)[grep("BH.rating.CD",names(BH))],"BH.CD.sum.SCORE")
  

  ####################################################################
  ########################### ANXIETY ################################
  ####################################################################
  AX = data.table(read_sav("savs/PAPA/PAPA_K5.sav"))
  
  ########################### PHOBIA ################################
  ax_cols = paste("K55",2:8,"1",sep = "_")
  AX[,PHOB.SYMPTOM.COUNT := sum(.SD > 1,na.rm = T),by = 1:nrow(AX), .SDcols = ax_cols]
  
  ax_imp_cols = paste("K55_26A",1:6,sep = "_")
  AX[,PHOB.IMPAIR.MISSING := sum(is.na(.SD)),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  AX[,PHOB.IMPAIRweak := sum(.SD == 1,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  AX[,PHOB.IMPAIRstrong := sum(.SD > 1,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
 
  AX[,PHOB.IMPAIR := 0]
  AX[PHOB.IMPAIR.MISSING >= 6, PHOB.IMPAIR := NA]
  AX[PHOB.IMPAIRweak > 2 | PHOB.IMPAIRstrong > 2, PHOB.IMPAIR := 1]
  AX[,PHOB.IMPAIR.CAT := factor(PHOB.IMPAIR,labels = c("absent","present"))]
  AX[,PHOB.IMPAIR.SCORE := sum(.SD,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  
  AX[,PHOB.GROUP := 4]
  AX[PHOB.SYMPTOM.COUNT > 1 & PHOB.IMPAIR == 1, PHOB.GROUP := 1]
  AX[PHOB.GROUP > 1 & PHOB.SYMPTOM.COUNT > 1 & PHOB.IMPAIR == 0, PHOB.GROUP := 2]
  AX[PHOB.GROUP > 2 & PHOB.SYMPTOM.COUNT > 1 & is.na(PHOB.IMPAIR), PHOB.GROUP := 3]
  AX[PHOB.GROUP > 3 & PHOB.SYMPTOM.COUNT < 1 , PHOB.GROUP := 4]
  my_labels = c("Phobia_clinical" = 1,"Phobia_subclinical" = 2,"Phobia_noimp" = 3,"no_Phobia" = 4)
  AX[,PHOB.GROUP := labelled(PHOB.GROUP,labels = my_labels)]

  ########################### SOC ANX ################################
  ax_cols = paste("K55",9:11,"1",sep = "_")
  AX[,SOAX.SYMPTOM.COUNT := sum(.SD > 1,na.rm = T),by = 1:nrow(AX), .SDcols = ax_cols]
  
  ax_imp_cols = paste("K55_26B",1:6,sep = "_")
  AX[,SOAX.IMPAIR.MISSING := sum(is.na(.SD)),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  AX[,SOAX.IMPAIRweak := sum(.SD == 1,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  AX[,SOAX.IMPAIRstrong := sum(.SD > 1,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  
  AX[,SOAX.IMPAIR := 0]
  AX[SOAX.IMPAIR.MISSING >= 6, SOAX.IMPAIR := NA]
  AX[SOAX.IMPAIR.MISSING < 6 & (SOAX.IMPAIRweak >= 2 | SOAX.IMPAIRstrong > 0) , SOAX.IMPAIR := 1]
  AX[,SOAX.IMPAIR.CAT := factor(SOAX.IMPAIR,labels = c("absent","present"))]
  AX[,SOAX.IMPAIR.SCORE := sum(.SD,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  
  AX[,SOAX.GROUP := 4]
  AX[K55_9_1 > 1 & SOAX.IMPAIR.CAT == "present", SOAX.GROUP := 1]
  AX[SOAX.GROUP > 1 & (
      (K55_9_1 > 1 & SOAX.IMPAIR.CAT == "absent") |
      (SOAX.SYMPTOM.COUNT >= 1 & SOAX.IMPAIR.CAT == "absent")
      ) , SOAX.GROUP := 2]
  AX[SOAX.GROUP > 2 & SOAX.SYMPTOM.COUNT >= 1 & is.na(SOAX.IMPAIR), SOAX.GROUP := 3]
  AX[SOAX.GROUP > 3 & SOAX.SYMPTOM.COUNT < 1, SOAX.GROUP := 4]
  my_labels = c("SocAnx_clinical" = 1,"SocAnx_subclinical" = 2,"SocAnx_noimp" = 3,"no_Phobia" = 4)
  AX[,SOAX.GROUP := labelled(SOAX.GROUP, labels =  my_labels)]
  
  ########################### SEP ANX ################################
  ax_cols = paste("K55",c(13,14,16:19),"1",sep = "_")
  AX[,SEPAX.SYMPTOM.COUNT := sum(.SD > 1,na.rm = T),by = 1:nrow(AX), .SDcols = ax_cols]

  ax_imp_cols = paste("K55_26C",1:6,sep = "_")
  AX[,SEPAX.IMPAIR.MISSING := sum(is.na(.SD)),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  AX[,SEPAX.IMPAIRweak := sum(.SD == 1,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  AX[,SEPAX.IMPAIRstrong := sum(.SD > 1,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  
  AX[,SEPAX.IMPAIR := 0]
  AX[SEPAX.IMPAIR.MISSING >= 6, SEPAX.IMPAIR := NA]
  AX[SEPAX.IMPAIR.MISSING < 6 & (SEPAX.IMPAIRweak >= 2 | SEPAX.IMPAIRstrong > 0) , SEPAX.IMPAIR := 1]
  AX[,SEPAX.IMPAIR.CAT := factor(SEPAX.IMPAIR,labels = c("absent","present"))]
  AX[,SEPAX.IMPAIR.SCORE := sum(.SD,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  
  my_labels = c("SEPAX_clinical" = 1,"SEPAX_subclinical" = 2,"SEPAX_noimp" = 3,"no_SEPAX" = 4)
  AX[SEPAX.SYMPTOM.COUNT > 3 & SEPAX.IMPAIR.CAT == "present", SEPAX.GROUP := 1]
  AX[is.na(SEPAX.GROUP) & (SEPAX.SYMPTOM.COUNT > 3 & SEPAX.IMPAIR.CAT == "absent" ) |
      (SEPAX.SYMPTOM.COUNT %in% 1:2 & is.na(SEPAX.IMPAIR)), SEPAX.GROUP := 2]
  AX[is.na(SEPAX.GROUP) & SEPAX.SYMPTOM.COUNT > 3 & is.na(SEPAX.IMPAIR), SEPAX.GROUP := 3]
  AX[is.na(SEPAX.GROUP), SEPAX.GROUP := 4]
  AX[,SEPAX.GROUP := labelled(SEPAX.GROUP,labels = my_labels)]
  
  ########################### GEN ANX ################################
  
  AX[,GENAX.SYMPT := 0]
  AX[K55_21_16 == 2, K55_21_16 := 1]
  AX[K55_20_10 == 2 & (K55_21_16 == 1 | K55_22_1 == 2 | K55_23_1 == 2 | K55_24_1 == 2 | K55_25_1 == 2),
     GENAX.SYMPT := 1]
  
  
  ax_imp_cols = paste("K55_26D",1:6,sep = "_")
  AX[,GENAX.IMPAIR.MISSING := sum(is.na(.SD)),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  AX[,GENAX.IMPAIRweak := sum(.SD == 1,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  AX[,GENAX.IMPAIRstrong := sum(.SD > 1,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  
  AX[,GENAX.IMPAIR := 0]
  AX[GENAX.IMPAIR.MISSING >= 6, GENAX.IMPAIR := NA]
  AX[GENAX.IMPAIR.MISSING < 6 & (GENAX.IMPAIRweak >= 2 | GENAX.IMPAIRstrong > 0) , GENAX.IMPAIR := 1]
  AX[,GENAX.IMPAIR.CAT := factor(GENAX.IMPAIR,labels = c("absent","present"))]
  AX[,GENAX.IMPAIR.SCORE := sum(.SD,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  
  my_la = c("GENAX_clinical" = 1,"GENAX_subclinical" = 2,"GENAX_noimp" = 3,"no_GENAX" = 4)
  AX[GENAX.SYMPT == 1 & GENAX.IMPAIR.CAT == "present", GENAX.GROUP := 1]
  AX[is.na(GENAX.GROUP) & GENAX.SYMPT == 1 & GENAX.IMPAIR.CAT == "absent", GENAX.GROUP := 2]
  AX[is.na(GENAX.GROUP) & GENAX.SYMPT == 1 & is.na(GENAX.IMPAIR.CAT), GENAX.GROUP := 3]
  AX[is.na(GENAX.GROUP) & GENAX.SYMPT == 0 , GENAX.GROUP := 4]
  AX[,GENAX.GROUP := labelled(GENAX.GROUP,labels = my_labels)]
  
  ########################### ANX GROUP ################################
  SDcols = paste(c("PHOB","SOAX","SEPAX","GENAX"),"GROUP",sep = ".")
  AX[,ANX.GROUP := min(.SD,na.rm = T),by = 1:nrow(AX),.SDcols = SDcols]
  my_labels = c("ANX_clinical" = 1,"ANX_subclin" = 2,"ANX_missimp" = 3,"no_ANX" = 4)
  AX[,ANX.GROUP := labelled(ANX.GROUP,labels = my_labels)]
  
 
  ########################### ANX sumscores ################################
  
  items2dims = list(PHOB = 2:8,
                    SOAX = 9:11,
                    SEPAX = c(13:14,16:19),
                    GENAX = 22:25)
  item2dimsAUTSYM = 16 # ANXIOUS AUTONOMIC SYMPTOMS 
  
  old_names = c(paste(paste("K55_",unlist(items2dims),sep = ""),"_1",sep = ""),
                paste("K55_21_",item2dimsAUTSYM,sep = ""))
  new_names = c(paste("ANX.rating.",names(unlist(items2dims)),sep = ""),
                paste("ANX.rating.AUTSYM",item2dimsAUTSYM,sep = ""))
  
  setnames(AX,old_names,new_names)
  
  for(v in new_names) {
    eval(parse(text = 
                 paste0("AX[",v," > 0, ",v," := ",v,"-1,by = list(PREG_ID_299,BARN_NR)]")
    ))
  } 
  
  AX = make_sum_scores(AX,names(AX)[grep("ANX.rating.PHOB",names(AX))],"ANX.rating.PHOB")
  AX = make_sum_scores(AX,names(AX)[grep("ANX.rating.SOAX",names(AX))],"ANX.rating.SOAX")
  AX = make_sum_scores(AX,names(AX)[grep("ANX.rating.SEPAX",names(AX))],"ANX.rating.SEPAX")
  AX = make_sum_scores(AX,names(AX)[grep("ANX.rating.GENAX",names(AX))],"ANX.rating.GENAX")
  
  AX$PREG_ID_299 = as.numeric(AX$PREG_ID_299)
  AX$BARN_NR = as.numeric(AX$BARN_NR)
  
  PAPA = merge(PAPA,AX,by = c("PREG_ID_299","BARN_NR"))
  
  ######################### konklusions ###################
  KK = data.table(read_sav("savs/PAPA/ADHD_KONKL.sav"))
  
  KK[, LANG.GROUP := 4]
  for (v in 3:1)  KK[KU2_1_1 == v | KU2_1_2 == v | KU2_2_1 == v, LANG.GROUP := v]
  my_labels = c( "LANG_DEV_PROBL_clin" = 1, "LANG_DEV_PROBL_subclin" = 2, "LANG_DEV_PROBL_lackinfo" = 3, "no_LANG_DEV_PROBL" = 4)
  KK[,LANG.GROUP := labelled(LANG.GROUP, labels = my_labels)]
  
  
  KK[, OTHER.GROUP := 4]
  for (v in 3:1) KK[KU4_1_1==v | KU6_1_1==v | KU6_1_2==v | KU6_3_1==v | KU6_3_2==v | KU6_3_3==v | KU7_1_1==v | KU7_1_2==v, OTHER.GROUP := v]
  my_labels = c( "OTHER_DISORDER_clin" = 1, "OTHER_DISORDER_subclin" = 2, "OTHER_DISORDER_lackinfo" = 3, "no_OTHER_DISORDER" = 4)
  KK[,OTHER.GROUP := labelled(OTHER.GROUP, labels = my_labels)]
  
  KK[, SLEEP.GROUP := 4]
  KK[KU7_3_7==2 | KU7_3_1==2 | KU7_3_2==2 | KU7_3_3==2 | KU7_3_8==2 | KU7_3_4==2 | KU7_3_6==2, SLEEP.GROUP := 2]
  KK[KU7_3_7==3 | KU7_3_1==3 | KU7_3_2==3 | KU7_3_3==3 | KU7_3_8==3 | KU7_3_4==3 | KU7_3_6==3, SLEEP.GROUP := 1]
  my_labels = c("SLEEP_REG_PROBL_wimp" = 1, "SLEEP_REG_PROBL_woimp" = 2, "no_SLEEP_REG_PROBL" = 4)
  KK[,SLEEP.GROUP := labelled(SLEEP.GROUP, labels = my_labels)]
  
  
  KK[, EMO.GROUP := 4]
  KK[KU8_1_1==2 | KU8_1_2==2 | KU8_1_3==2 | KU8_1_4==2 | KU8_1_5==2 | KU8_1_6==2, EMO.GROUP := 2]
  KK[KU8_1_1==3 | KU8_1_2==3 | KU8_1_3==3 | KU8_1_4==3 | KU8_1_5==3 | KU8_1_6==3, EMO.GROUP := 1]
  my_labels = c("EMOT_DYSREG_wimp" = 1, "EMOT_DYSREG_woimp" = 2, "EMOT_DYSREG_PROBL" = 4)
  KK[,EMO.GROUP := labelled(EMO.GROUP, labels = my_labels)]
  
  KK[, XOTHER.GROUP := 2]
  KK[OTHER.GROUP < 3 | SLEEP.GROUP < 3 | EMO.GROUP < 3,XOTHER.GROUP := 1]
  KK[,XOTHER.GROUP := labelled(XOTHER.GROUP, labels = c("Yes" = 1, "No" = 2))]
  
  
  ############################# merge and ADHD COMORBIDITIES ##############################
  
  PAPA = merge(AD[,names(AD)[c(1:2,grep("^ADHD",names(AD)))],with = F],
               BH[,names(BH)[c(1:2,grep("^BH",names(BH)))],with = F],
               by = c("PREG_ID_299","BARN_NR"))
  PAPA = merge(PAPA,
               AX[,names(AX)[c(1:2,grep("^ANX",names(AX)))],with = F],
               by = c("PREG_ID_299","BARN_NR"))
  PAPA = merge(PAPA,
               SL[,names(SL)[c(1:2,grep("^SLEEP",names(SL)))],with = F],
               by = c("PREG_ID_299","BARN_NR"))
  
  PAPA = merge(PAPA,
               KK[,names(KK)[c(1:2,grep("GROUP$",names(KK)))],with = F],
               by = c("PREG_ID_299","BARN_NR"))
  
  PAPA[,PAPA.DIAG.GROUP := 4]
  PAPA[ADHD.CATEGORY <= 2 & BH.DBD.GROUP == 4 & ANX.GROUP == 4, PAPA.DIAG.GROUP := 1]
  PAPA[PAPA.DIAG.GROUP > 1 & ADHD.CATEGORY <= 2 & (BH.DBD.GROUP < 4 | ANX.GROUP < 4), PAPA.DIAG.GROUP := 2]
  PAPA[PAPA.DIAG.GROUP > 2 & ADHD.CATEGORY == 3 & (BH.DBD.GROUP < 4 | ANX.GROUP < 4), PAPA.DIAG.GROUP := 3]
  PAPA[PAPA.DIAG.GROUP > 3 & ADHD.CATEGORY == 3 & BH.DBD.GROUP == 4 & ANX.GROUP == 4, PAPA.DIAG.GROUP := 4]
  my_labels = c("ADHD_only" = 1, "ADHD_w_COMROB" = 2, "OTHER_only" = 3, "no_Diagnosis" = 4)
  PAPA[,PAPA.DIAG.GROUP := labelled(PAPA.DIAG.GROUP,labels = my_labels)]
 
    
  PAPA[,PAPA.DIAG.GROUP2 := 4]
  PAPA[ADHD.CATEGORY <= 2 & XOTHER.GROUP == 2, PAPA.DIAG.GROUP2 := 1]
  PAPA[ADHD.CATEGORY <= 2 & BH.DBD.GROUP <  3 & ANX.GROUP <  3 & LANG.GROUP < 3 & XOTHER.GROUP == 1 & PAPA.DIAG.GROUP2 > 1, PAPA.DIAG.GROUP2 := 2]
  PAPA[ADHD.CATEGORY > 2 & BH.DBD.GROUP <  3 & ANX.GROUP <  3 & LANG.GROUP < 3 & XOTHER.GROUP == 1 & PAPA.DIAG.GROUP2 > 2, PAPA.DIAG.GROUP2 := 3]
  PAPA[ADHD.CATEGORY > 2 & BH.DBD.GROUP ==  3 & ANX.GROUP ==  3 & LANG.GROUP > 3 & XOTHER.GROUP == 2 & PAPA.DIAG.GROUP2 > 3, PAPA.DIAG.GROUP2 := 3]
  PAPA[,PAPA.DIAG.GROUP2 := labelled(PAPA.DIAG.GROUP2,labels = my_labels)]
  
  
  IF (ADHDkategori LE 2) AND (((DBD_Group EQ 4) AND (ANX_Group EQ 4) AND (K_LDgrp GE 3) AND (Andreprob EQ 2)) OR (Andreprob EQ 2)) Grupper = 1.
  
  return(PAPA)
}


