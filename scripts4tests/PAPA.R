

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
  SL = data.table(read_sav("F:/Forskningsprosjekter/PDB 299 - ADHD-studien Prescho_/Forskningsfiler/GUBI/GuidoData/masterfile/savs/PAPA/PAPA_K2.sav"))
  
  old_names = c("K2_2_1_1_1", "K2_2_1_1_2", "K2_2_1_1_3","K2_2_1_2_1",
                "K2_2_1_3_1", "K2_2_1_3_2_1", "K2_2_1_3_2_2", "K2_2_1_3_2_3", "K2_2_1_3_2_4",
                "K2_2_1_4_1","K2_2_1_5_1","K2_2_1_6_1","K2_2_1_6_2",
                "K2_2_1_8_1","K2_2_1_8_2","K2_2_1_10_1","K2_2_1_11_1",
                "K2_2_1_12_1","K2_2_1_13_1","K2_2_1_14_1","K2_2_1_16_1")
  # btr = beadtime ritual, slp = sleep, slpr = sleeper, slps = sleeps,unr_a_slp = unrested after sleep
  # slps_day = sleeps during day, wk_night = wakes at night, d = day, e = evening, diff_slp = difficulties sleeping
  new_names = c("got2bed","get_up","h_night","res_slp",
                "btr","btr_read","btr_hist","btr_sing","btr_oth",
                "leaves_bed","wk_night","t_asleep","diff_slp",
                "self_slpr_d","self_slpr_e","restl_sleep","unr_a_slp",
                "Hypsomn","slps_d","nightm","somnabul")
  new_names = paste("S.",new_names,sep = "")
  setnames(SL,old_names,new_names)
  
  SL = SL[,c(names(SL)[1:2],new_names),with = F]
  
  d = SL$S.h_night
  d = gsub("[ a-z]","",d)
  d = gsub(",",".",d)
  d[d == "??"] = NA
  d[d == "11-12"]= 11.5
  d[d == "10-11"]= 10.5
  d[d == "1015"]= 10.15
  for(k in  grep("-",d))  d[k] = mean(as.numeric(unlist(strsplit(d[k],"-"))))
  
  d = as.numeric(d)
  SL$S.h_night = d
  
  t = SL$S.t_asleep
  t = gsub("[ a-z]","",t)
  t = gsub(",",".",t)
  for(k in  grep("-",t))  t[k] = mean(as.numeric(unlist(strsplit(t[k],"-"))))
  t = as.numeric(t)
  t = SL$S.t_asleep
  SL$S.t_asleep = t
  SL = SL[,c(1,2,grep("S",names(SL))),with = F]
  rm(d,k,old_names,new_names,t)
  ################################ ADHD #########################################
  AD = data.table(read_sav("F:/Forskningsprosjekter/PDB 299 - ADHD-studien Prescho_/Forskningsfiler/GUBI/GuidoData/masterfile/savs/PAPA/PAPA_K3.sav"))
  
  #*(Collaps "selvstendig" and "voksenstyrt" activity into variable K33_10).
  AD[K33_10_1 >= 2 | K33_10_2 >= 2, K33_10 := 2 ]
  
  hyp_cols = paste("K33",c(1,3:5,7,8),"1",sep = "_")
  AD[,ADHD.HY.SC := sum(.SD > 1,na.rm = T),by = 1:nrow(AD),.SDcols = hyp_cols]
  imp_cols = paste("K33",c(21:23),"1",sep = "_")
  AD[,ADHD.IM.SC := sum(.SD > 1,na.rm = T),by = 1:nrow(AD),.SDcols = imp_cols]
  att_cols = c("K33_10",paste("K33",c(11:18),"1",sep = "_"))
  AD[,ADHD.AT.SC := sum(.SD > 1,na.rm = T),by = 1:nrow(AD),.SDcols = att_cols]
  AD[,ADHD.HI.SC :=  ADHD.HY.SC + ADHD.IM.SC]
  AD[,ADHD.SC :=  ADHD.HY.SC + ADHD.IM.SC + ADHD.AT.SC]
  
  ######################## Impairment score ########################
  imp_cols = paste("K33_28",1:6,sep = "_")
  AD[,ADHD.IMP.MI := sum(.SD,na.rm = T),by = 1:nrow(AD),.SDcols = imp_cols]
  AD[,ADHD.IMPwk := sum(.SD == 1,na.rm = T),by = 1:nrow(AD),.SDcols = imp_cols]
  AD[,ADHD.IMPstr := sum(.SD > 1,na.rm = T),by = 1:nrow(AD),.SDcols = imp_cols]
  
  AD[,ADHD.IMP := 0]
  AD[ADHD.IMP.MI >= 6, ADHD.IMP := NA]
  AD[ADHD.IMP.MI < 6 & 
       (ADHD.IMPwk >= 2 | ADHD.IMPstr > 0) , ADHD.IMP := 1]
  AD[,ADHD.IMP.CAT := factor(ADHD.IMP,labels = c("absent","present"))]
  
  AD[,ADHD.IMP.SS := sum(.SD,na.rm = T),by = 1:nrow(AD),.SDcols = imp_cols]
  
  ######################## ADHD sub groups ########################
  my_labels = c("ADHD_IA" = 1,"ADHD_H" = 2,"ADHD_C" = 3,"no_ADHD" = 4)
  AD[ADHD.IMP.CAT == "present" & ADHD.AT.SC >= 6 & ADHD.HI.SC < 6, ADHD.SGR := 1]
  AD[ADHD.IMP.CAT == "present" & ADHD.AT.SC < 6 & ADHD.HI.SC >= 6, ADHD.SGR := 2]
  AD[ADHD.IMP.CAT == "present" & ADHD.AT.SC >= 6 & ADHD.HI.SC >= 6, ADHD.SGR := 3]
  AD[ADHD.IMP.CAT == "absent" | (ADHD.AT.SC < 6 & ADHD.HI.SC < 6), ADHD.SGR := 4]
  AD[,ADHD.SGR := labelled(ADHD.SGR,labels = my_labels)]
  
  ######################## ADHD subthreshold without impairment ########################
  my_labels = c("ADHD_subthr_woi_IA" = 1,"ADHD_subthr_woi_H" = 2,"ADHD_subthr_woi_C" = 3,"no_ADHD_subthr_woi" = 4)
  AD[ADHD.SGR == 4 & ADHD.AT.SC >= 6 & ADHD.HI.SC < 6, ADHD.SGR.ST_woi := 1]
  AD[ADHD.SGR == 4 & ADHD.AT.SC < 6 & ADHD.HI.SC >= 6, ADHD.SGR.ST_woi := 2]
  AD[ADHD.SGR == 4 & ADHD.AT.SC >= 6 & ADHD.HI.SC >= 6, ADHD.SGR.ST_woi := 3]
  AD[ADHD.SGR == 4 & ADHD.AT.SC < 6 & ADHD.HI.SC < 6, ADHD.SGR.ST_woi := 4]
  AD[,ADHD.SGR.ST_woi := labelled(ADHD.SGR.ST_woi,labels = my_labels)]
  
  ######################## ADHD subthreshold with impairment ########################
  my_labels = c("ADHD_subthr_wi_IA" = 1,"ADHD_subthr_wi_H" = 2,"ADHD_subthr_wi_C" = 3,"no_ADHD_subthr_wi" = 4)
  AD[ADHD.SGR == 4 & (ADHD.AT.SC > 2 & ADHD.AT.SC < 6)  & ADHD.HI.SC < 3 & ADHD.IMP.CAT == "present", ADHD.SGR.ST_wi := 1]
  AD[ADHD.SGR == 4 & ADHD.AT.SC < 3 & (ADHD.HI.SC > 2 & ADHD.HI.SC < 6) & ADHD.IMP.CAT == "present", ADHD.SGR.ST_wi := 2]
  AD[ADHD.SGR == 4 & (ADHD.AT.SC > 2 & ADHD.AT.SC < 6) & (ADHD.HI.SC > 2 & ADHD.HI.SC < 6) & ADHD.IMP.CAT == "present", ADHD.SGR.ST_wi := 3]
  AD[ADHD.SGR == 4 & ADHD.AT.SC < 3 & ADHD.HI.SC < 3, ADHD.SGR.ST_wi := 4]
  AD[,ADHD.SGR.ST_wi := labelled(ADHD.SGR.ST_wi,labels = my_labels)]
  
  ######################## Combined variable for subthreshold ADHD ########################
  my_labels = c("ADHD_subthr_IA" = 1,"ADHD_subthr_H" = 2,"ADHD_subthr_C" = 3,"no_ADHD_subthr" = 4)
  AD[ADHD.SGR.ST_woi == 1 | ADHD.SGR.ST_wi == 1, ADHD.SGR.ST := 1]
  AD[is.na(ADHD.SGR.ST) & ADHD.SGR.ST_woi == 2 | ADHD.SGR.ST_wi == 2, ADHD.SGR.ST := 2]
  AD[is.na(ADHD.SGR.ST) & ADHD.SGR.ST_woi == 3 | ADHD.SGR.ST_wi == 3, ADHD.SGR.ST := 3]
  AD[is.na(ADHD.SGR.ST) , ADHD.SGR.ST := 4]
  AD[,ADHD.SGR.ST := labelled(ADHD.SGR.ST,labels = my_labels)]
  
  my_labels = c("ADHD_c_clin" = 1,"ADHD_H_clin" = 2,"ADHD_IA_clin" = 3,"ADHD_C_subclin" = 4,"ADHD_H_subclin" = 5,"ADHD_IA_subclin" = 6,"no_ADHD" = 7)
  AD[ADHD.SGR < 4, ADHD.GR := abs(ADHD.SGR-4)]
  AD[ADHD.SGR > 3 & ADHD.SGR.ST < 4, ADHD.GR := abs(ADHD.SGR.ST-4)+3]
  AD[is.na(ADHD.GR), ADHD.GR := 7]
  AD[,ADHD.GR := labelled(ADHD.GR,labels = my_labels)]
  
  ############################ ADHD CAT #############################
  AD[ADHD.GR <= 3 ,ADHD.CAT := 1]
  AD[ADHD.GR > 3 & ADHD.GR < 7 ,ADHD.CAT := 2]
  AD[is.na(ADHD.CAT) ,ADHD.CAT := 3]
  my_labels = c("ADHD_clin" = 1, "ADHD_subclin" = 2, "no_ADHD" = 3)
  AD[,ADHD.CAT := labelled(ADHD.CAT,labels = my_labels)]
  
  ############################ ADHD sum scores #############################
  items2dims = list(HY = c(1,3,4,5,7,8),
                    IM = c(21,22,23),
                    AT = c(10:18))
  old_names = paste(paste("K33_",unlist(items2dims),sep = ""),"_1",sep = "")
  new_names = paste("ADHD.",names(unlist(items2dims)),".SS",sep = "")
  setnames(AD,old_names,new_names)
  AD = make_sum_scores(AD,names(AD)[grep("ADHD.HY[0-9].SS",names(AD))],"ADHD.HY.SS")
  AD = make_sum_scores(AD,names(AD)[grep("ADHD.IM[0-9].SS",names(AD))],"ADHD.IM.SS")
  AD = make_sum_scores(AD,names(AD)[grep("ADHD.AT[0-9].SS",names(AD))],"ADHD.AT.SS")
  SDcols = c("ADHD.HY.SS" , "ADHD.IM.SS" , "ADHD.AT.SS")
  AD[,ADHD.SS := sum(.SD),by = 1:nrow(AD),.SDcols = SDcols]
  
  AD[,PREG_ID_299 := as.numeric(PREG_ID_299)]
  AD[,BARN_NR := as.numeric(BARN_NR)]
  
  rm(att_cols,hyp_cols,imp_cols,items2dims,new_names,old_names)
  
  AD = AD[,c(1,2,grep("ODD|CD|ADHD",names(AD))),with = F]
  #######################################################################
  ############################### BH: ODD ###############################
  #######################################################################
  
  BH = data.table(read_sav("F:/Forskningsprosjekter/PDB 299 - ADHD-studien Prescho_/Forskningsfiler/GUBI/GuidoData/masterfile/savs/PAPA/PAPA_K4.sav"))
  # SYM duration
  freq_vars = sort(c(paste("K44",c(3,4,6,9,12),"2",sep = "_"),paste("K44",c(10,11),"6",sep = "_"),"K44_8_4"))
  for (v in freq_vars){
    BH[,tmp := char2num(get(v)),by = 1:nrow(BH)]
    BH[tmp > 90,tmp := 90]
    BH[tmp < 1,tmp := NA]
    eval(parse(text=paste0("BH[,",v,":=NULL]")))
    setnames(BH,"tmp",v)
  }
  
  
  # SYM present
  ## SYM til stede defineres som skåre 2 + høy frekvens.
  BH[K44_3_1 > 1 & K44_3_2 >= 90,K44_3_1F := K44_3_1]
  BH[K44_4_1 > 1 & K44_4_2 >= 48,K44_4_1F := K44_4_1]
  BH[K44_6_1 > 1 & K44_6_2 >= 85,K44_6_1F := K44_6_1]
  BH[K44_8_1 > 1 & K44_8_4 >= 44,K44_8_1F := K44_8_1]
  BH[K44_9_1 > 1 & K44_9_2 >= 1 ,K44_9_1F := K44_9_1]
  BH[K44_10_1 > 1 & K44_10_6 >= 1,K44_10_1F := K44_10_1]
  BH[K44_11_1 > 1 & K44_11_6 >= 21 ,K44_11_1F := K44_11_1]
  BH[K44_12_1 > 1 & K44_12_2 >= 1,K44_12_1F := K44_12_1]
  
  
  # * Her telles antall ODD-SYMer med hyppighet tatt i betraktning.
  SDcols = names(BH)[intersect(grep("^K44",names(BH)),grep("1F$",names(BH)))]
  BH[,ODD.SC := sum(.SD > 1,na.rm = T),by = 1:nrow(BH),.SDcols = SDcols]
  
  # * Her telles antall ODD-SYMer uavhengig av hyppighet av forekomst.
  SDcols =  gsub("_2|_6|_4","_1",freq_vars)
  BH[,ODD.SYwofreq := sum(.SD > 1,na.rm = T),by = 1:nrow(BH),.SDcols = SDcols]
  BH[,ODD.SYwofreq.MI := sum(is.na(.SD)),by = 1:nrow(BH),.SDcols = SDcols]
  
  # impairment   
  impair_cols = paste("K44_23A",1:6,sep = "_")
  BH[,ODD.IMP.MI := sum(is.na(.SD)),by = 1:nrow(BH), .SDcols = impair_cols]
  BH[,ODD.IMP.wk := sum(.SD == 1,na.rm = T),by = 1:nrow(BH), .SDcols = impair_cols]
  BH[,ODD.IMPstr := sum(.SD > 1,na.rm = T),by = 1:nrow(BH), .SDcols = impair_cols]
  
  BH[, ODD.IMP := 0]
  BH[(ODD.IMP.MI < 6 & ODD.IMP.wk > 2) | ODD.IMPstr > 0, ODD.IMP := 1]
  BH[ODD.IMP.MI >= 6, ODD.IMP := NA]
  
  SDcols = paste("K44_23A",1:6,sep = "_")
  BH[,ODD.IMP.SS := sum(.SD,na.rm = T),by = 1:nrow(BH),.SDcols = SDcols]
  BH[,ODD.IMP.SS.MI := sum(is.na(.SD)),by = 1:nrow(BH),.SDcols = SDcols]
  BH[ODD.IMP.SS.MI > 2, ODD.IMP.SS := NA]
  
  
  SDcols = paste("K44",c(3,4,6,8,9,10,11,12),"1F",sep = "_")
  BH[,ODD.SC := sum(.SD > 0,na.rm = T),by = 1:nrow(BH), .SDcols = SDcols]
  
  BH[, ODD.GR := 5]
  BH[ODD.SC >= 4 & ODD.IMP == 1, ODD.GR := 1]
  BH[ODD.GR > 1 & 
       ((ODD.SC > 4 & ODD.IMP == 0) |
          ((ODD.SC > 0 & ODD.SC < 4) & ODD.IMP == 1)), ODD.GR := 2]
  BH[ODD.GR > 2 & ODD.SC >= 4 & is.na(ODD.IMP), ODD.GR := 3]
  BH[ODD.GR > 3 & 
       (ODD.SC <  4 & (ODD.IMP == 0 | is.na(ODD.IMP))) | 
       ODD.SC == 0, ODD.GR := 4]
  BH[ODD.GR == 5, ODD.GR := NA]
  
  my_labels = c("ODD_clin" = 1, "ODD_subclin" = 2, "ODD_missimp" = 3, "no_ODD" = 4)
  BH[,ODD.GR := labelled(ODD.GR,labels = my_labels)]
  
  rm(freq_vars,impair_cols,SDcols,v)
  
  ######################################################################
  ############################### BH: CD ###############################
  ######################################################################
  #BH$K44_14_1 = 2*(BH$K44_14_1 > = 2 | BH$K44_15_1 >= 2) #(Collaps "erter" and "mobber" into variable K44_14).
  #BH$K44_19_1 = 2*(BH$K44_19_1 > = 2 | BH$K44_20_1 >= 2) #(Collaps "sloss" and "angrep" into variable K44_19).
  labels_14 = c(0,2,3)
  names(labels_14) = paste(names(attributes(BH[["K44_14_1"]])$labels),names(attributes(BH[["K44_15_1"]])$labels),sep = " / ")
  labels_19 = c(0,2,3)
  names(labels_19) = paste(names(attributes(BH[["K44_19_1"]])$labels),names(attributes(BH[["K44_20_1"]])$labels),sep = " / ")
  
  BH$K44_14_1 = as.numeric(cut(BH$K44_15_1 + BH$K44_14_1,breaks = c(-1,1,3.5,6.5)))
  BH$K44_14_1[BH$K44_14_1<2] = 0
  BH$K44_19_1 = as.numeric(cut(BH$K44_20_1 + BH$K44_19_1,breaks = c(-1,1,3.5,6.5)))
  BH$K44_19_1[BH$K44_19_1<2] = 0
  BH$K44_13_1[BH$K44_13_1 == 9] = NA # 9 was for "snakker ikke" in a question about lying
  
  BH[["K44_14_1"]] = labelled(BH[["K44_14_1"]],labels = labels_14)
  BH[["K44_19_1"]] = labelled(BH[["K44_19_1"]],labels = labels_19)
  
  BH[,CD.SC := sum(.SD > 1,na.rm = T),by = 1:nrow(BH),
     .SDcols = c("K44_13_1", "K44_16_1", "K44_17_1", "K44_18_1", "K44_21_1", "K44_22_1", "K44_14")]
  BH[,CD.SY.MI := sum(is.na(.SD),na.rm = T),by = 1:nrow(BH),
     .SDcols = c("K44_13_1", "K44_16_1", "K44_17_1", "K44_18_1", "K44_21_1", "K44_22_1", "K44_14")]
  BH[CD.SY.MI > 3, CD.SC := NA]
  
  cd.imp.cols = paste("K44_23B",1:6,sep = "_") 
  BH[,CD.IMP.MI := sum(.SD,na.rm = T),by = 1:nrow(BH),.SDcols = cd.imp.cols]
  BH[,CD.IMPwk := sum(.SD == 1,na.rm = T),by = 1:nrow(BH),.SDcols = cd.imp.cols]
  BH[,CD.IMPstr := sum(.SD > 1,na.rm = T),by = 1:nrow(BH),.SDcols = cd.imp.cols]
  
  BH[,CD.IMP := 0]
  BH[CD.IMP.MI >= 6, CD.IMP := NA]
  BH[CD.IMP.MI < 6 & 
       (CD.IMPwk >= 2 | CD.IMPstr > 0) , CD.IMP := 1]
  BH[,CD.IMP.CAT := factor(CD.IMP,labels = c("absent","present"))]
  
  BH[,CD.IMP.SS := sum(.SD,na.rm = T),by = 1:nrow(BH),.SDcols = cd.imp.cols]
  
  BH[,CD.GR := 4]
  BH[CD.SC >= 3 & CD.IMP == 1,CD.GR := 1]
  BH[CD.GR > 1 & 
       (CD.SC >= 3 & CD.IMP == 0) | 
       ((CD.SC == 1 | CD.SC == 2)  & CD.IMP == 1),
     CD.GR := 2]
  BH[CD.GR > 2 & CD.SC >= 3 & is.na(CD.IMP), CD.GR := 3]
  BH[is.na(CD.SC) & is.na(CD.IMP), CD.GR := NA]
  
  my_labels = c("CD_clin" = 1, "CD_subclin" = 2, "CD_missimp" = 3, "no_CD" = 4)
  BH[,CD.GR := labelled(CD.GR,labels = my_labels)]
  
  
  rm(cd.imp.cols)
  
  
  BH[,DBD.GR := 4]
  BH[ODD.GR == 1 | CD.GR == 1, DBD.GR := 1]
  BH[DBD.GR > 1 & (ODD.GR == 2 | CD.GR == 2), DBD.GR := 2]
  BH[DBD.GR > 2 & (ODD.GR == 3 | CD.GR == 3), DBD.GR := 3]
  
  my_labels = c("DBD_clin" = 1, "DBD_subclin" = 2, "DBD_missimp" = 3, "no_DBD" = 4)
  BH[,DBD.GR := labelled(DBD.GR,labels = my_labels)]
  
  
  BH$PREG_ID_299 = as.numeric(BH$PREG_ID_299)
  BH$BARN_NR = as.numeric(BH$BARN_NR)
  
  items2dims = list(ODD = c(3,4,6,8,9,10,11,12),
                    CD = c(13,14,16,17,18,19,21,22))
  
  old_names = paste(paste("K44_",unlist(items2dims),sep = ""),"_1",sep = "")
  new_names = paste(names(unlist(items2dims)),".SS",sep = "")
  setnames(BH,old_names,new_names)
  for(v in new_names) {
    eval(parse(text = 
                 paste0("BH[",v," > 0, ",v," := ",v,"-1,by = list(PREG_ID_299,BARN_NR)]")
    ))
  } 
  
  BH = make_sum_scores(BH,names(BH)[grep("ODD[0-9].SS",names(BH))],"ODD.SS")
  BH = make_sum_scores(BH,names(BH)[grep("CD[0-9].SS",names(BH))],"CD.SS")
  
  
  BH = BH[,c(1,2,grep("ODD|CD|DBD",names(BH))),with = F]
  ####################################################################
  ########################### ANXIETY ################################
  ####################################################################
  AX = data.table(read_sav("F:/Forskningsprosjekter/PDB 299 - ADHD-studien Prescho_/Forskningsfiler/GUBI/GuidoData/masterfile/savs/PAPA/PAPA_K5.sav"))
  
  ########################### PHOBIA ################################
  ax_cols = paste("K55",2:8,"1",sep = "_")
  AX[,PHO.SC := sum(.SD > 1,na.rm = T),by = 1:nrow(AX), .SDcols = ax_cols]
  
  ax_imp_cols = paste("K55_26A",1:6,sep = "_")
  AX[,PHO.IMP.MI := sum(is.na(.SD)),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  AX[,PHO.IMPwk := sum(.SD == 1,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  AX[,PHO.IMPstr := sum(.SD > 1,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  
  AX[,PHO.IMP := 0]
  AX[PHO.IMP.MI >= 6, PHO.IMP := NA]
  AX[PHO.IMPwk > 2 | PHO.IMPstr > 2, PHO.IMP := 1]
  AX[,PHO.IMP.CAT := factor(PHO.IMP,labels = c("absent","present"))]
  AX[,PHO.IMP.SS := sum(.SD,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  
  AX[,PHO.GR := 4]
  AX[PHO.SC > 1 & PHO.IMP == 1, PHO.GR := 1]
  AX[PHO.GR > 1 & PHO.SC > 1 & PHO.IMP == 0, PHO.GR := 2]
  AX[PHO.GR > 2 & PHO.SC > 1 & is.na(PHO.IMP), PHO.GR := 3]
  AX[PHO.GR > 3 & PHO.SC < 1 , PHO.GR := 4]
  my_labels = c("Phobia_clinical" = 1,"Phobia_subclinical" = 2,"Phobia_noimp" = 3,"no_Phobia" = 4)
  AX[,PHO.GR := labelled(PHO.GR,labels = my_labels)]
  
  ########################### SOC ANX ################################
  ax_cols = paste("K55",9:11,"1",sep = "_")
  AX[,SOA.SC := sum(.SD > 1,na.rm = T),by = 1:nrow(AX), .SDcols = ax_cols]
  
  ax_imp_cols = paste("K55_26B",1:6,sep = "_")
  AX[,SOA.IMP.MI := sum(is.na(.SD)),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  AX[,SOA.IMPwk := sum(.SD == 1,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  AX[,SOA.IMPstr := sum(.SD > 1,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  
  AX[,SOA.IMP := 0]
  AX[SOA.IMP.MI >= 6, SOA.IMP := NA]
  AX[SOA.IMP.MI < 6 & (SOA.IMPwk >= 2 | SOA.IMPstr > 0) , SOA.IMP := 1]
  AX[,SOA.IMP.CAT := factor(SOA.IMP,labels = c("absent","present"))]
  AX[,SOA.IMP.SS := sum(.SD,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  
  AX[,SOA.GR := 4]
  AX[K55_9_1 > 1 & SOA.IMP.CAT == "present", SOA.GR := 1]
  AX[SOA.GR > 1 & (
    (K55_9_1 > 1 & SOA.IMP.CAT == "absent") |
      (SOA.SC >= 1 & SOA.IMP.CAT == "absent")
  ) , SOA.GR := 2]
  AX[SOA.GR > 2 & SOA.SC >= 1 & is.na(SOA.IMP), SOA.GR := 3]
  AX[SOA.GR > 3 & SOA.SC < 1, SOA.GR := 4]
  my_labels = c("SocAnx_clinical" = 1,"SocAnx_subclinical" = 2,"SocAnx_noimp" = 3,"no_Phobia" = 4)
  AX[,SOA.GR := labelled(SOA.GR, labels =  my_labels)]
  
  ########################### SEP ANX ################################
  ax_cols = paste("K55",c(13,14,16:19),"1",sep = "_")
  AX[,SEA.SC := sum(.SD > 1,na.rm = T),by = 1:nrow(AX), .SDcols = ax_cols]
  
  ax_imp_cols = paste("K55_26C",1:6,sep = "_")
  AX[,SEA.IMP.MI := sum(is.na(.SD)),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  AX[,SEA.IMPwk := sum(.SD == 1,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  AX[,SEA.IMPstr := sum(.SD > 1,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  
  AX[,SEA.IMP := 0]
  AX[SEA.IMP.MI >= 6, SEA.IMP := NA]
  AX[SEA.IMP.MI < 6 & (SEA.IMPwk >= 2 | SEA.IMPstr > 0) , SEA.IMP := 1]
  AX[,SEA.IMP.CAT := factor(SEA.IMP,labels = c("absent","present"))]
  AX[,SEA.IMP.SS := sum(.SD,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  
  my_labels = c("SEA_clinical" = 1,"SEA_subclinical" = 2,"SEA_noimp" = 3,"no_SEA" = 4)
  AX[SEA.SC > 3 & SEA.IMP.CAT == "present", SEA.GR := 1]
  AX[is.na(SEA.GR) & (SEA.SC > 3 & SEA.IMP.CAT == "absent" ) |
       ((SEA.SC == 1 | SEA.SC == 2) & is.na(SEA.IMP)), SEA.GR := 2]
  AX[is.na(SEA.GR) & SEA.SC > 3 & is.na(SEA.IMP), SEA.GR := 3]
  AX[is.na(SEA.GR), SEA.GR := 4]
  AX[,SEA.GR := labelled(SEA.GR,labels = my_labels)]
  
  ########################### GEN ANX ################################
  
  AX[,GEA.SY := 0]
  AX[K55_21_16 == 2, K55_21_16 := 1]
  AX[K55_20_10 == 2 & (K55_21_16 == 1 | K55_22_1 == 2 | K55_23_1 == 2 | K55_24_1 == 2 | K55_25_1 == 2),
     GEA.SY := 1]
  
  
  ax_imp_cols = paste("K55_26D",1:6,sep = "_")
  AX[,GEA.IMP.MI := sum(is.na(.SD)),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  AX[,GEA.IMPwk := sum(.SD == 1,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  AX[,GEA.IMPstr := sum(.SD > 1,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  
  AX[,GEA.IMP := 0]
  AX[GEA.IMP.MI >= 6, GEA.IMP := NA]
  AX[GEA.IMP.MI < 6 & (GEA.IMPwk >= 2 | GEA.IMPstr > 0) , GEA.IMP := 1]
  AX[,GEA.IMP.CAT := factor(GEA.IMP,labels = c("absent","present"))]
  AX[,GEA.IMP.SS := sum(.SD,na.rm = T),by = 1:nrow(AX), .SDcols = ax_imp_cols]
  
  my_la = c("GEA_clinical" = 1,"GEA_subclinical" = 2,"GEA_noimp" = 3,"no_GEA" = 4)
  AX[GEA.SY == 1 & GEA.IMP.CAT == "present", GEA.GR := 1]
  AX[is.na(GEA.GR) & GEA.SY == 1 & GEA.IMP.CAT == "absent", GEA.GR := 2]
  AX[is.na(GEA.GR) & GEA.SY == 1 & is.na(GEA.IMP.CAT), GEA.GR := 3]
  AX[is.na(GEA.GR) & GEA.SY == 0 , GEA.GR := 4]
  AX[,GEA.GR := labelled(GEA.GR,labels = my_labels)]
  
  ########################### ANX GR ################################
  SDcols = paste(c("PHO","SOA","SEA","GEA"),"GR",sep = ".")
  AX[,ANX.GR := min(.SD,na.rm = T),by = 1:nrow(AX),.SDcols = SDcols]
  my_labels = c("ANX_clinical" = 1,"ANX_subclin" = 2,"ANX_missimp" = 3,"no_ANX" = 4)
  AX[,ANX.GR := labelled(ANX.GR,labels = my_labels)]
  
  
  ########################### ANX sumscores ################################
  
  items2dims = list(PHO = 2:8,
                    SOA = 9:11,
                    SEA = c(13:14,16:19),
                    GEA = 22:25)
  item2dimsAUTSYM = 16 # ANXIOUS AUTONOMIC SYMS 
  
  old_names = c(paste(paste("K55_",unlist(items2dims),sep = ""),"_1",sep = ""),
                paste("K55_21_",item2dimsAUTSYM,sep = ""))
  new_names = c(paste(names(unlist(items2dims)),".SS",sep = ""),
                paste("AUT",item2dimsAUTSYM,".SS",sep = ""))
  
  setnames(AX,old_names,new_names)
  
  for(v in new_names) {
    eval(parse(text = 
                 paste0("AX[",v," > 0, ",v," := ",v,"-1,by = list(PREG_ID_299,BARN_NR)]")
    ))
  } 
  
  AX = make_sum_scores(AX,names(AX)[grep("PHO[0-9].SS",names(AX))],"PHO.SS")
  AX = make_sum_scores(AX,names(AX)[grep("SEA[0-9].SS",names(AX))],"SEA.SS")
  AX = make_sum_scores(AX,names(AX)[grep("GEA[0-9].SS",names(AX))],"GEA.SS")
  AX = make_sum_scores(AX,names(AX)[grep("SOA[0-9].SS",names(AX))],"SOA.SS")
  
  AX$PREG_ID_299 = as.numeric(AX$PREG_ID_299)
  AX$BARN_NR = as.numeric(AX$BARN_NR)
  
  AX = AX[,c(1,2,grep("SOA|GEA|SEA|PHO|ANX",names(AX))),with = F]
  
  ######################### konklusions ###################
  KK = data.table(read_sav("F:/Forskningsprosjekter/PDB 299 - ADHD-studien Prescho_/Forskningsfiler/GUBI/GuidoData/masterfile/savs/PAPA/ADHD_KONKL.sav"))
  
  KK[, LANG.GR := 4]
  for (v in 3:1)  KK[KU2_1_1 == v | KU2_1_2 == v | KU2_2_1 == v, LANG.GR := v]
  my_labels = c( "LANG_DEV_PROBL_clin" = 1, "LANG_DEV_PROBL_subclin" = 2, "LANG_DEV_PROBL_lackinfo" = 3, "no_LANG_DEV_PROBL" = 4)
  KK[,LANG.GR := labelled(LANG.GR, labels = my_labels)]
  
  
  KK[, OTHER.GR := 4]
  for (v in 3:1) KK[KU4_1_1==v | KU6_1_1==v | KU6_1_2==v | KU6_3_1==v | KU6_3_2==v | KU6_3_3==v | KU7_1_1==v | KU7_1_2==v, OTHER.GR := v]
  my_labels = c( "OTHER_DISORDER_clin" = 1, "OTHER_DISORDER_subclin" = 2, "OTHER_DISORDER_lackinfo" = 3, "no_OTHER_DISORDER" = 4)
  KK[,OTHER.GR := labelled(OTHER.GR, labels = my_labels)]
  
  KK[, SL.GR := 4]
  KK[KU7_3_7==2 | KU7_3_1==2 | KU7_3_2==2 | KU7_3_3==2 | KU7_3_8==2 | KU7_3_4==2 | KU7_3_6==2, SL.GR := 2]
  KK[KU7_3_7==3 | KU7_3_1==3 | KU7_3_2==3 | KU7_3_3==3 | KU7_3_8==3 | KU7_3_4==3 | KU7_3_6==3, SL.GR := 1]
  my_labels = c("SLEEP_REG_PROBL_wimp" = 1, "SLEEP_REG_PROBL_woimp" = 2, "no_SLEEP_REG_PROBL" = 4)
  KK[,SL.GR := labelled(SL.GR, labels = my_labels)]
  
  
  KK[, EMO.GR := 4]
  KK[KU8_1_1==2 | KU8_1_2==2 | KU8_1_3==2 | KU8_1_4==2 | KU8_1_5==2 | KU8_1_6==2, EMO.GR := 2]
  KK[KU8_1_1==3 | KU8_1_2==3 | KU8_1_3==3 | KU8_1_4==3 | KU8_1_5==3 | KU8_1_6==3, EMO.GR := 1]
  my_labels = c("EMOT_DYSREG_wimp" = 1, "EMOT_DYSREG_woimp" = 2, "EMOT_DYSREG_PROBL" = 4)
  KK[,EMO.GR := labelled(EMO.GR, labels = my_labels)]
  
  KK[, XOTHER.GR := 2]
  KK[OTHER.GR < 3 | SL.GR < 3 | EMO.GR < 3,XOTHER.GR := 1]
  KK[,XOTHER.GR := labelled(XOTHER.GR, labels = c("Yes" = 1, "No" = 2))]
  
  KK = KK[,c(1,2,grep("LANG|EMO|OTHER|SL|XOTHER",names(KK))),with = F]
  
  ############################# merge and ADHD COMORBIDITIES ##############################
  
  PAPA = merge(AD, BH, by = c("PREG_ID_299","BARN_NR"))
  PAPA = merge(PAPA, AX, by = c("PREG_ID_299","BARN_NR"))
  PAPA = merge(PAPA, SL, by = c("PREG_ID_299","BARN_NR"))
  PAPA = merge(PAPA, KK, by = c("PREG_ID_299","BARN_NR"))
  
  PAPA[,DIAG.GR := 4]
  PAPA[ADHD.CAT <= 2 & DBD.GR == 4 & ANX.GR == 4, DIAG.GR := 1]
  PAPA[DIAG.GR > 1 & ADHD.CAT <= 2 & (DBD.GR < 4 | ANX.GR < 4), DIAG.GR := 2]
  PAPA[DIAG.GR > 2 & ADHD.CAT == 3 & (DBD.GR < 4 | ANX.GR < 4), DIAG.GR := 3]
  PAPA[DIAG.GR > 3 & ADHD.CAT == 3 & DBD.GR == 4 & ANX.GR == 4, DIAG.GR := 4]
  my_labels = c("ADHD_only" = 1, "ADHD_w_COMROB" = 2, "OTHER_only" = 3, "no_Diagnosis" = 4)
  PAPA[,DIAG.GR := labelled(DIAG.GR,labels = my_labels)]
  
  
  
  ############################### Add labels to variable names ############################
  
  # abbreviations used in variables: 
  # SL = sleep
  # - SOA = social anxiety
  # - SEA = separation anxiety
  # - GEA = generalized anxiety
  # - PHO = phobia
  # - ANX = any anxiety
  # - AT = inattention
  # - HY = hyperactivity
  # - IM = impulsivity
  # - HI = hyperactivity and impulsivity
  # - CD = conduct disorder
  # - ODD = oppositional defient disorder
  # - DBD =  disruptive behavior disorders
  # - SC = symptom count, number of present symptoms
  # - SS = symptom score, severity of symptoms (typically 3 levels, 0, 2, 3)
  # - IMP = impairement (weak impairement = IMPwk, strong impairement = IMPstr)
  # - MI = number of missing values (typically for a variable ..IMP)
  # - GR = group, typically diagnostic group
  # - SGR = sub group, typically diagnostic group
  # - ST = sub threshold (typically sub threshold symptoms)
  # - CAT = category (often diagnostic category)
  # - subthr = sub threshold
  # - wi & woi = with & without impairment
  # - clin = clinical
  # - PP = PAPA (Preschool Age Psychiatric Assessment Interview)
  # - SY = symptom
  
  attributes(PAPA$ADHD.SC) = list(label = "PAPA: Total number of present ADHD SYMPTOMs")
  attributes(PAPA$ADHD.AT.SC) = list(label = "PAPA: Number of present inattentiveness SYMPTOMs")
  attributes(PAPA$ADHD.HY.SC) = list(label = "PAPA: Number of present hyperactivity SYMPTOMs")
  attributes(PAPA$ADHD.IM.SC) = list(label = "PAPA: Number of present impulsivity SYMPTOMs")
  attributes(PAPA$ADHD.HI.SC) = list(label = "PAPA: Number of present hyperactivity & impulsiveness SYMPTOMs")
  
  translate = c(SEA = "separation anxiety", 
                IMP = "impairments", 
                SS = "sum of scores",
                SL = "sleep",
                SOA = "social anxiety",
                GEA = "generalized anxiety",
                PHO = "phobia",
                ANX = "any anxiety",
                AT = "inattention",
                HY = "hyperactivity",
                IM = "impulsivity",
                HI = "hyperactivity and impulsivity",
                CD = "conduct disorder",
                ODD = "oppositional defient disorder",
                DBD =  "disruptive behavior disorders",
                SC = "number of present symptoms",
                MI = "number of missing values",
                GR = "group",
                SGR = "sub group",
                ST = "sub threshold",
                CAT = "category",
                clin = "clinical",
                SY = "symptom",
                ADHD = "ADHD",
                IMPwk = "weak impairment",
                IMPstr = "strong impairment",
                ST_woi = "sub threshold without impairment",
                ST_wi = "sub threshold without impairment",
                ODD1 = "Defiance",
                ODD2 = "Argues with adults",
                ODD3 = "Angry outbursts",
                ODD4 = "Annoys others on purpose",
                ODD5 = "Blame others",
                ODD6 = "Malicious or vindictive",
                ODD7 = "Touchy, does not cope with much from others",
                ODD8 = "Angry and annoyed",
                CD1 = "Lies",
                CD2 = "Teasing/Bullying",
                CD3 = "Cruel to animals",
                CD4 = "Mean/cruel to humans",
                CD5 = "Steals",
                CD6 = "Fighting/Attacking",
                CD7 = "Destroys property",
                CD8 = "Arsony/plays with fire",
                PHO1 = "Fear of animals",
                PHO2 = "Fear of storms, thunder and/or lightning",
                PHO3 = "Fear for doctor or dentist",
                PHO4 = "Fear of blood/needles",
                PHO5 = "Fear of the dark",
                PHO6 = "Fear of activities in small spaces",
                PHO7 = "Fear of xxxx",
                SOA1 = "Social anxiety",
                SOA2 = "Fear of activities in public areas",
                SOA3 = "FEar of social situations with adults",
                SEA1 = "Avoids being alone",
                SEA2 = "Pre concern/resistance to being separated",
                SEA3 = "Nightmares about separation",
                SEA4 = "Scared/anxious about attending kindergarden",
                SEA5 = "Fear of possible injury",
                SEA6 = "Resistance to going to sleep alone",
                GEA1 = "Jumpy",
                GEA2 = "Trouble concentrating when anxious",
                GEA3 = "Easily tired when anxious/worried",
                GEA4 = "Exaggerated need for reassurance",
                HY1 = "Physical restlessness",
                HY2 = "Hard time sitting still",
                HY3 = "Runs/climbs too much",
                HY4 = "Always on the run",
                HY5 = "Speaks unusually much",
                HY6 = "Hard time doing things calmly",
                AT1 = "Hard time doing independent tasks or play activities",
                AT2 = "Hard time organizing tasks",
                AT3 = "Hard time following instructions",
                AT4 = "Avoids mentally challenging tasks",
                AT5 = "Easily distracted",
                AT6 = "FOrgetful with daily chores",
                AT7 = "Often drops things",
                AT8 = "Do not listen",
                AT9 = "Is unprecise",
                IM1 = "Hard time waiting for his/her turn",
                IM2 = "Blurts out answers to questions",
                IM3 = "Often interrupts or disturbs others",
                SYwofreq = "symptoms without frequency",
                Wk = "weak",
                btr = "bedtime ritual",
                btr_read = "bedtime ritual, reading",
                btr_hist = "bedtime ritual, history/conversation",
                btr_sing = "bedtime ritual, singing",
                btr_oth = "bedtime ritual, other",
                S = "sleep",
                slp = "sleep",
                slpr = "sleeper",
                slps = "sleeps",
                unr_a_slp = "unrested after sleep",
                slps_d = "sleeps during day",
                wk_night = "wakes at night",
                d = "day",
                e = "evening",
                diff_slp = "difficulties sleeping",
                Oth = "other",
                self_slpr_d = "Cradle oneself to sleep at daytime",
                self_slpr_e = "Cradle oneself to sleep in the evening",
                restl_sleep = "restless sleep",
                Hypsomn = "Increased need for sleep/hypersomnia",
                nightm = "nightmares",
                DIAG = "diagnostic",
                LANG = "language",
                OTHER = "other",
                t_asleep = "How much time does it thake for the child to og to sleep?",
                got2bed = "When does the child go to bed?",
                get_up = "When does the child usually get up in the morning?",
                leaves_bed = "Leaves bed before going to sleep",
                EMO = "affective disorder",
                somnabul = "Somnabulisme",
                XOTHER = "Other disorder than ADHD",
                res_slp = "resistance to go to sleep",
                h_night = "hours sleep per night")
  
  for (variable in names(PAPA)[-c(1,2)]){
    variable_info = strsplit(variable,split = "\\.")[[1]]
    if (length(translate[variable_info]) == length(variable_info)){
      label = paste0("PAPA: ", paste(translate[variable_info],collapse = "; "))
      if ( is(PAPA[[variable]],"labelled") ) {
        attributes(PAPA[[variable]])[["label"]] = label
      } else {
        attributes(PAPA[[variable]]) = list(label = label)
      }
    }
  }
  
  lapply(PAPA,function(x) attributes(x)[["label"]])
  
  return(PAPA)
}

