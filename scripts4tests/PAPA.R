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
  
  SL1 = NaN2NA(data.table(read_sav(paste0(data_dir,"PAPA_K2_1_V1.sav"))))
  SL1 = merge(SL1,NaN2NA(data.table(read_sav(paste0(data_dir,"PAPA_K2_2_V1.sav")))), by = index_vars)
  SL1 = merge(SL1,NaN2NA(data.table(read_sav(paste0(data_dir,"PAPA_K2_3_V1.sav")))), by = index_vars)
  SL1 = merge(SL1,NaN2NA(data.table(read_sav(paste0(data_dir,"PAPA_K2_4_V1.sav")))), by = index_vars)
  SL1[,AlderUtfylt_PAPA_K2 := AlderUtfylt_PAPA_K2_3_V1]
  SL1[,K2_INSTRUMENT_ID_V2 := K2_INSTRUMENT_ID.x]
  SL1[,(unique(names(SL1)[grep(".x$|.y$",names(SL1))])) := NULL]
  SL1[,(unique(names(SL1)[grep(".x$|.y$",names(SL1))])) := NULL]
  SL2 = NaN2NA(data.table(read_sav(paste0(data_dir,"PAPA_K2_V2.sav"))))
  SL2[,AlderUtfylt_PAPA_K2 := AlderUtfylt_PAPA_K2_V2]
  
  for(v in intersect(names(SL1),names(SL2))) {
    if (class(SL1[[v]]) == "numeric" & class(SL2[[v]]) == "labelled") {
      SL1[[v]] = labelled(SL1[[v]],labels = attr(SL2[[v]],"labels"))
    } else if (class(SL1[[v]]) == "character" & class(SL2[[v]]) == "numeric") {
      if ( (mean(SL1[[v]] == "") > .5 & mean(is.na(SL2[[v]])) > .5) |
           mean(SL2[[v]],na.rm = T) > 1200) {
        SL1[[v]] = as.numeric(SL1[[v]])
        attr(SL1[[v]],"label") = attributes(SL2[[v]])[["label"]]
      }
    }
    if (class(SL1[[v]]) != class(SL2[[v]]) ) {
      SL1[[v]] = unlist(apply(data.frame(as.vector(SL1[[v]])),1, make_numbers))
    }
  }
  
  
  SL = rbind(SL1[,(intersect(names(SL1),names(SL2))),with = F],
             SL2[,(intersect(names(SL1),names(SL2))),with = F])
  
  save(SL,file = paste0(data_dir,"PAPA_K2.Rdata"))
  
  for (v in index_vars) SL[, c(v) := as.numeric(get(v))]
  
  old_names = c("K2_2_1_1_1", "K2_2_1_1_2", "K2_2_1_1_3","K2_2_1_2_1",
                "K2_2_1_3_1", "K2_2_1_3_2_1", "K2_2_1_3_2_2", "K2_2_1_3_2_3", "K2_2_1_3_2_4",
                "K2_2_1_4_1","K2_2_1_5_1","K2_2_1_6_1","K2_2_1_6_2",
                "K2_2_1_8_1","K2_2_1_8_2","K2_2_1_10_1","K2_2_1_11_1",
                "K2_2_1_12_1","K2_2_1_13_1","K2_2_1_14_1","K2_2_1_16_1")
  # btr = beadtime ritual, slp = sleep, slpr = sleeper, slps = sleeps,unr_a_slp = unrested after sleep
  # slps_day = sleeps during day, wk_night = wakes at night, d = day, e = evening, diff_slp = difficulties sleeping
  new_names = c("go2bed","get_up","h_night","res_slp",
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
  
  SL[S.get_up == "19:00", S.get_up := "07:00"]
  SL[S.get_up == "19:30", S.get_up := "07:30"]
  SL[S.get_up == "16:30", S.get_up := "06:30"]
  SL[S.get_up == "16.30", S.get_up := "06:30"]
  SL[S.get_up == "20:00", S.get_up := "08:00"]
  SL[S.get_up == "18:15", S.get_up := "06:15"]
  SL[S.get_up == "07-0800", S.get_up := "07-08"]
  SL[S.get_up == "070", S.get_up := "07"]
  SL[,S.get_up := get_time(S.get_up)]
  
  SL[S.go2bed == "1930-20", S.go2bed := "19:30-20:00"]
  SL[S.go2bed == "1900-2000", S.go2bed := "19:00-20:00"]
  SL[S.go2bed == "8", S.go2bed := "20:00"]
  SL[S.go2bed == "8:00", S.go2bed := "20:00"]
  SL[S.go2bed == "08:00", S.go2bed := "20:00"]
  SL[,S.go2bed := gsub("07","19",S.go2bed)]
  SL[,S.go2bed := get_time(S.go2bed)]
  
  
  SL[,S.t_asleep := gsub("[a-z]","",S.t_asleep)]
  SL[,S.t_asleep := gsub(",",",",S.t_asleep)]
  for (k in grep("-",SL$S.t_asleep)) {
    SL$S.t_asleep[k] = as.character(mean(as.numeric(strsplit(SL$S.t_asleep[k],"-")[[1]])))
  }
  SL[,S.t_asleep := as.numeric(S.t_asleep)]
  
  ################################ emotion regulation  ############################
  load(paste0(data_dir,"PAPA_K2.Rdata"))
  REG = SL
  for (v in index_vars) REG[, c(v) := as.numeric(get(v))]
  
  old_names = c("K2_2_2_1_1", "K2_2_2_2_1", "K2_2_2_3_1", "K2_2_2_4_1", "K2_2_2_5_1")
  new_names = paste0("ER.i",1:length(old_names))
  setnames(REG,old_names,new_names)
  REG = make_sum_scores(REG,new_names,"PP.ER.SS")
  
  ################################ sensoric reactivity  ############################
  old_names = paste0("K2_2_3_",1:10,"_1")
  new_names = paste0("SR.i",1:length(old_names))
  setnames(REG,old_names,new_names)
  REG = make_sum_scores(REG,new_names,"SR.SS")
  
  ################################ eating habits reactivity ############################
  old_names = c("K2_2_4_1_1", "K2_2_4_1_2", "K2_2_4_1_3","K2_2_4_2_1")
  new_names = paste0("EH.i",1:length(old_names))
  setnames(REG,old_names,new_names)
  REG = make_sum_scores(REG,new_names,"EH.SS")
  
  REG = REG[,c(index_vars,names(REG)[grep("^EH.|^ER.|^SR.",names(REG))]),with = F]
  
  ###############################################################################
  ################################ ADHD #########################################
  ###############################################################################
  AD = NaN2NA(data.table(read_sav(paste0(data_dir,"PAPA_K3.sav"))))
  for (v in index_vars) AD[, c(v) := as.numeric(get(v))]
  
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
  AD[,ADHD.IMP.MI := sum(is.na(.SD)),by = 1:nrow(AD),.SDcols = imp_cols]
  AD[,ADHD.IMPwk := sum(.SD == 1,na.rm = T),by = 1:nrow(AD),.SDcols = imp_cols]
  AD[,ADHD.IMPstr := sum(.SD > 1,na.rm = T),by = 1:nrow(AD),.SDcols = imp_cols]
  
  AD[,ADHD.IMP := 0]
  AD[ADHD.IMP.MI >= 6, ADHD.IMP := NA]
  AD[ADHD.IMP.MI < 6 & 
       (ADHD.IMPwk >= 2 | ADHD.IMPstr > 0) , ADHD.IMP := 1]
  AD[,ADHD.IMP.CAT := factor(ADHD.IMP,labels = c("absent","present"))]
  
  AD[,ADHD.IMP.SS := sum(.SD,na.rm = T),by = 1:nrow(AD),.SDcols = imp_cols]
  AD[is.na(ADHD.IMP.CAT),ADHD.IMP.SS := NA]
  
  ######################## ADHD sub groups ########################
  my_labels = c("ADHD_IA" = 1,"ADHD_H" = 2,"ADHD_C" = 3,"no_ADHD" = 4)
  AD[ADHD.IMP.CAT == "present" & ADHD.AT.SC >= 6 & ADHD.HI.SC < 6, ADHD.SG := 1]
  AD[ADHD.IMP.CAT == "present" & ADHD.AT.SC < 6 & ADHD.HI.SC >= 6, ADHD.SG := 2]
  AD[ADHD.IMP.CAT == "present" & ADHD.AT.SC >= 6 & ADHD.HI.SC >= 6, ADHD.SG := 3]
  AD[ADHD.IMP.CAT == "absent" | (ADHD.AT.SC < 6 & ADHD.HI.SC < 6), ADHD.SG := 4]
  AD[,ADHD.SG := labelled(ADHD.SG,labels = my_labels)]
  
  ######################## ADHD subthreshold without impairment ########################
  my_labels = c("ADHD_subthr_woi_IA" = 1,"ADHD_subthr_woi_H" = 2,"ADHD_subthr_woi_C" = 3,"no_ADHD_subthr_woi" = 4)
  AD[ADHD.SG == 4 & ADHD.AT.SC >= 6 & ADHD.HI.SC < 6, ADHD.SG.ST_woi := 1]
  AD[ADHD.SG == 4 & ADHD.AT.SC < 6 & ADHD.HI.SC >= 6, ADHD.SG.ST_woi := 2]
  AD[ADHD.SG == 4 & ADHD.AT.SC >= 6 & ADHD.HI.SC >= 6, ADHD.SG.ST_woi := 3]
  AD[ADHD.SG == 4 & ADHD.AT.SC < 6 & ADHD.HI.SC < 6, ADHD.SG.ST_woi := 4]
  AD[,ADHD.SG.ST_woi := labelled(ADHD.SG.ST_woi,labels = my_labels)]
  
  ######################## ADHD subthreshold with impairment ########################
  my_labels = c("ADHD_subthr_wi_IA" = 1,"ADHD_subthr_wi_H" = 2,"ADHD_subthr_wi_C" = 3,"no_ADHD_subthr_wi" = 4)
  AD[ADHD.SG == 4 & (ADHD.AT.SC > 2 & ADHD.AT.SC < 6)  & ADHD.HI.SC < 3 & ADHD.IMP.CAT == "present", ADHD.SG.ST_wi := 1]
  AD[ADHD.SG == 4 & ADHD.AT.SC < 3 & (ADHD.HI.SC > 2 & ADHD.HI.SC < 6) & ADHD.IMP.CAT == "present", ADHD.SG.ST_wi := 2]
  AD[ADHD.SG == 4 & (ADHD.AT.SC > 2 & ADHD.AT.SC < 6) & (ADHD.HI.SC > 2 & ADHD.HI.SC < 6) & ADHD.IMP.CAT == "present", ADHD.SG.ST_wi := 3]
  AD[ADHD.SG == 4 & ADHD.AT.SC < 3 & ADHD.HI.SC < 3, ADHD.SG.ST_wi := 4]
  AD[,ADHD.SG.ST_wi := labelled(ADHD.SG.ST_wi,labels = my_labels)]
  
  ######################## Combined variable for subthreshold ADHD ########################
  my_labels = c("ADHD_subthr_IA" = 1,"ADHD_subthr_H" = 2,"ADHD_subthr_C" = 3,"no_ADHD_subthr" = 4)
  AD[ADHD.SG.ST_woi == 1 | ADHD.SG.ST_wi == 1, ADHD.SG.ST := 1]
  AD[is.na(ADHD.SG.ST) & ADHD.SG.ST_woi == 2 | ADHD.SG.ST_wi == 2, ADHD.SG.ST := 2]
  AD[is.na(ADHD.SG.ST) & ADHD.SG.ST_woi == 3 | ADHD.SG.ST_wi == 3, ADHD.SG.ST := 3]
  AD[is.na(ADHD.SG.ST) , ADHD.SG.ST := 4]
  AD[,ADHD.SG.ST := labelled(ADHD.SG.ST,labels = my_labels)]
  
  my_labels = c("ADHD_c_clin" = 1,"ADHD_H_clin" = 2,"ADHD_IA_clin" = 3,"ADHD_C_subclin" = 4,"ADHD_H_subclin" = 5,"ADHD_IA_subclin" = 6,"no_ADHD" = 7)
  AD[ADHD.SG < 4, ADHD.GR := abs(ADHD.SG-4)]
  AD[ADHD.SG > 3 & ADHD.SG.ST < 4, ADHD.GR := abs(ADHD.SG.ST-4)+3]
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
  new_names = paste0("ADHD.",
                     gsub("[0-9]","",names(unlist(items2dims))),
                     ".i",
                     gsub("[A-Z]","",names(unlist(items2dims))))
  setnames(AD,old_names,new_names)
  AD = make_sum_scores(AD,names(AD)[grep("ADHD.HY.i[0-9]",names(AD))],"ADHD.HY.SS")
  AD = make_sum_scores(AD,names(AD)[grep("ADHD.IM.i[0-9]",names(AD))],"ADHD.IM.SS")
  AD = make_sum_scores(AD,names(AD)[grep("ADHD.AT.i[0-9]",names(AD))],"ADHD.AT.SS")
  
  AD = make_sum_scores(AD,
                       names(AD)[grep("ADHD.AT.i[0-9]|ADHD.IM.i[0-9]|ADHD.HY.i[0-9]",names(AD))],
                       "ADHD.SS",count_score = F)
  
  rm(att_cols,hyp_cols,imp_cols,items2dims,new_names,old_names)
  
  AD = AD[,c(1,2,grep("ADHD",names(AD))),with = F]
  
  #######################################################################
  ############################### BH: ODD ###############################
  #######################################################################
  
  BH = NaN2NA(data.table(read_sav(paste0(data_dir,"PAPA_K4.sav"))))
  for (v in index_vars) BH[, c(v) := as.numeric(get(v))]
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
  BH[,ODD.IMPwk := sum(.SD == 1,na.rm = T),by = 1:nrow(BH), .SDcols = impair_cols]
  BH[,ODD.IMPstr := sum(.SD > 1,na.rm = T),by = 1:nrow(BH), .SDcols = impair_cols]
  
  BH[, ODD.IMP := 0]
  BH[(ODD.IMP.MI < 6 & ODD.IMPwk > 2) | ODD.IMPstr > 0, ODD.IMP := 1]
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
  
  BH[,K44_14_1 := as.numeric(cut(K44_15_1 + K44_14_1,breaks = c(-1,1,3.5,6.5)))]
  BH[K44_14_1<2, K44_14_1 := 0]
  BH[, K44_14_1:= labelled(K44_14_1,labels = labels_14)]
  BH[,K44_19_1 := as.numeric(cut(K44_19_1 + K44_20_1,breaks = c(-1,1,3.5,6.5)))]
  BH[K44_19_1<2, K44_19_1 := 0]
  BH[, K44_19_1:= labelled(K44_19_1,labels = labels_19)]
  
  BH[,CD.SC := sum(.SD > 1,na.rm = T),by = 1:nrow(BH),
     .SDcols = c("K44_13_1", "K44_16_1", "K44_17_1", "K44_18_1", "K44_21_1", "K44_22_1", "K44_14_1")]
  BH[,CD.SY.MI := sum(is.na(.SD),na.rm = T),by = 1:nrow(BH),
     .SDcols = c("K44_13_1", "K44_16_1", "K44_17_1", "K44_18_1", "K44_21_1", "K44_22_1", "K44_14_1")]
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
  
  items2dims = list(ODD = c(3,4,6,8,9,10,11,12),
                    CD = c(13,14,16,17,18,19,21,22))
  
  old_names = paste(paste("K44_",unlist(items2dims),sep = ""),"_1",sep = "")
  
  new_names = paste0(gsub("[0-9]","",names(unlist(items2dims))),
                     ".i",
                     gsub("[A-Z]","",names(unlist(items2dims))))
  
  setnames(BH,old_names,new_names)
  
  for(v in new_names) {
    eval(parse(text = 
                 paste0("BH[",v," > 0, ",v," := ",v,"-1,by = index_vars]")
    ))
  } 
  
  BH = make_sum_scores(BH,names(BH)[grep("ODD.i[0-9]",names(BH))],"ODD.SS")
  BH = make_sum_scores(BH,names(BH)[grep("CD.i[0-9]",names(BH))],"CD.SS")
  
  
  BH = BH[,c(1,2,grep("ODD|CD|DBD",names(BH))),with = F]
  ####################################################################
  ########################### ANXIETY ################################
  ####################################################################
  AX = NaN2NA(data.table(read_sav(paste0(data_dir,"PAPA_K5.sav"))))
  for (v in index_vars) AX[, c(v) := as.numeric(get(v))]
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
  
  items2dims[["ATS"]] = 16
  new_names = paste0(gsub("[0-9]","",names(unlist(items2dims))),
                     ".i",
                     gsub("[A-Z]","",names(unlist(items2dims))))
  
  
  setnames(AX,old_names,new_names)
  
  for(v in new_names) {
    eval(parse(text = 
                 paste0("AX[",v," > 0, ",v," := ",v,"-1,by = index_vars]")
    ))
  } 
  
  AX = make_sum_scores(AX,names(AX)[grep("PHO.i[0-9]",names(AX))],"PHO.SS")
  AX = make_sum_scores(AX,names(AX)[grep("SEA.i[0-9]",names(AX))],"SEA.SS")
  #AX = make_sum_scores(AX,names(AX)[grep("GEA.i[0-9]",names(AX))],"GEA.SS")
  AX = make_sum_scores(AX,names(AX)[grep("SOA.i[0-9]",names(AX))],"SOA.SS")
  
  AX = AX[,c(1,2,grep("SOA|GEA|SEA|PHO|ANX",names(AX))),with = F]
  
  #########################################################
  ######################### konklusions ###################
  #########################################################
  
  KK = NaN2NA(data.table(read_sav(paste0(data_dir,"ADHD_KONKL.sav"))))
  
  labels = c(KU1_4_1 = "Neuropsychological evaluation",
             KU2_1_1 = "Expressive speach problems",
             KU2_1_2 = "Combined expressive and receptive speach problems",
             KU2_2_1 = "Phonological speach problems",
             KU4_1_1 = "Motorkoordination problems",
             KU4_3_1 = "ADHD combined type",
             KU4_3_2 = "ADHD inattentive type",
             KU4_3_3 = "ADHD hyperative/imulsive type",
             KU4_3_4 = "Attention-problems and Hyperactivity",
             KU5_1_1 = "Oppositional defient disorder (ODD)",
             KU5_1_2 = "Conduct disorder (CD)",
             KU5_3_1 = "Separation anxiety",
             KU5_3_2 = "Specific phobia",
             KU5_3_3 = "Social phobia",
             KU5_3_4 = "Generalized anxiety",
             KU5_3_5 = "Selective mutism",
             KU6_1_1 = "Adaptation disoder",
             KU6_1_2 = "PTSD",
             KU6_3_1 = "Tourette syndrome",
             KU6_3_2 = "Chronic motor/vocal tics",
             KU6_3_3 = "Transient tics",
             KU7_1_1 = "Autism",
             KU7_1_2 = "Severe developmental retardation/disorder",
             KU7_3_7 = "Problems falling to sleep",
             KU7_3_1 = "Sleeps to little",
             KU7_3_2 = "Sleeps to much",
             KU7_3_3 = "Unusul sleeping during the day with, exhausted",
             KU7_3_4 = "Nightmares",
             KU7_3_5 = "Nightscares",
             KU7_3_6 = "Somnambulism",
             KU8_1_1 = "Attachement problems",
             KU8_1_2 = "Compulsive behaviors",
             KU8_1_3 = "Sadness (depression)",
             KU8_1_4 = "Regulation difficulties - Mood",
             KU8_1_5 = "Regulation difficulties - Sensorik",
             KU8_1_6 = "Regulation difficulties - Eating")
  
  vnames = c("NeuroPsych","ExpSpeach","ExReSpeach","PhonSpeach","Motor","ADHD.C","ADHD.A","ADHD.HI","AttHyp","ODD","CD",
             "SepAnx","SpecPhob","SocPhob","GenAnx","SelMut","AdptDis","PTSD","Toure","ChronTic","TransTic","Autism",
             "DevRet","Sleep","SleepL","SleepM","Exhaust","Nighm","Nightsc","Somn","Attachm","Compuls","SadDep",
             "RegMood","RegSens","RegEat")
  
  
  diags = 1:23
  diffs = 24:length(labels)
  
  #labels[diags] = paste0("PAPA conlusion; Diagnosis: ",labels[diags])
  #labels[diffs] = paste0("PAPA conclusion; Diffiulty; ",labels[diffs])
  
  KK[,(names(labels)) := lapply(.SD, function(x) {x[is.na(x)] = 0; return(x)}), .SDcols = names(labels)]
  
  translate_labels = function(v) {
    v = gsub("Utilstrekkelig informasjon","Insufficient Information",v)
    v = gsub("Subterskel fenomener","subclinical symptoms",v)
    v = gsub("Klinisk diagnose","clinical diagnosis",v)
    v = gsub("Fenomen tilstede uten impact","Symptoms without impact",v)
    v = gsub("Fenomen tilstede med impact","Symptoms with impact",v)
  }
  
  for (vx in c(names(labels)[diags],names(labels)[diffs])) {
    names(attr(KK[[vx]],"labels")) = translate_labels(names(attr(KK[[vx]],"labels")))
    if(min(attr(KK[[vx]],"labels")) > 0 & min(KK[[vx]]) == 0) {
      if(vx %in% names(labels)[diags]) {
        attr(KK[[vx]],"labels") = c(c("no diagnosis" = 0), attr(KK[[vx]],"labels"))
      } else {
        attr(KK[[vx]],"labels") = c(c("no symptoms" = 0), attr(KK[[vx]],"labels"))
      }
    }
  }
  
  tmp = KK[,c("KU2_1_1", "KU2_1_2", "KU2_2_1"),with = F]
  KK[,kLANG.GR := apply(tmp,1,function(x) max(x,na.rm = T))]
  rm(tmp)
  my_labels = c( "no_LANG_DEV_PROBL" = 0,
                 "LANG_DEV_PROBL_lackinfo" = 1,
                 "LANG_DEV_PROBL_subclin" = 2,
                 "LANG_DEV_PROBL_clin" = 3)
  KK[,kLANG.GR := labelled(kLANG.GR, labels = my_labels)]
  
  
  KK[, kOTHER.GR := 0]
  for (v in 3:1) KK[KU4_1_1==v | KU6_1_1==v | KU6_1_2==v | KU6_3_1==v | KU6_3_2==v | KU6_3_3==v | KU7_1_1==v | KU7_1_2==v, kOTHER.GR := v]
  my_labels = c("no_OTHER_DISORDER" = 0, "OTHER_DISORDER_clin" = 1, "OTHER_DISORDER_subclin" = 2, "OTHER_DISORDER_lackinfo" = 3)
  KK[,kOTHER.GR := labelled(kOTHER.GR, labels = my_labels)]
  
  KK[, kSL.GR := 0]
  KK[KU7_3_7==2 | KU7_3_1==2 | KU7_3_2==2 | KU7_3_3==2 | KU7_3_8==2 | KU7_3_4==2 | KU7_3_6==2, kSL.GR := 2]
  KK[KU7_3_7==3 | KU7_3_1==3 | KU7_3_2==3 | KU7_3_3==3 | KU7_3_8==3 | KU7_3_4==3 | KU7_3_6==3, kSL.GR := 1]
  my_labels = c("no_SLEEP_REG_PROBL" = 0, "SLEEP_REG_PROBL_wimp" = 1, "SLEEP_REG_PROBL_woimp" = 2)
  KK[,kSL.GR := labelled(kSL.GR, labels = my_labels)]
  
  
  KK[, kEMO.GR := 0]
  KK[KU8_1_1==2 | KU8_1_2==2 | KU8_1_3==2 | KU8_1_4==2 | KU8_1_5==2 | KU8_1_6==2, kEMO.GR := 2]
  KK[KU8_1_1==3 | KU8_1_2==3 | KU8_1_3==3 | KU8_1_4==3 | KU8_1_5==3 | KU8_1_6==3, kEMO.GR := 1]
  my_labels = c("EMOT_DYSREG_PROBL" = 0, "EMOT_DYSREG_wimp" = 1, "EMOT_DYSREG_woimp" = 2)
  KK[,kEMO.GR := labelled(kEMO.GR, labels = my_labels)]
  
  KK[, kXOTHER.GR := 2]
  KK[kOTHER.GR < 3 | kSL.GR < 3 | kEMO.GR < 3,kXOTHER.GR := 1]
  KK[,kXOTHER.GR := labelled(kXOTHER.GR, labels = c("Yes" = 1, "No" = 2))]
  
  
  new_names = c(paste0("DIA.",vnames[diags]),
                paste0("DIF.",vnames[diffs]))
  setnames(KK,names(labels),new_names)
  
  keep_vars = c(index_vars,
                new_names,
                names(KK)[grep("LANG|EMO|OTHER|SL|XOTHER",names(KK))])
  
  KK = KK[,keep_vars,with = F]
  
  diag_labels = labels
  names(diag_labels) = vnames
  
  SDcols = c("DIA.ADHD.A","DIA.ADHD.HI","DIA.ADHD.C")
  KK[,DIA.ADHD.any_ADHD := labelled(max(.SD),
                                    labels = attr(KK$DIA.ADHD.A,"labels")),
     by = 1:nrow(KK),
     .SDcols = SDcols]
  
  ######################### communication, social play, rep. beh. #################
  
  CSR = NaN2NA(data.table(read_sav(paste0(data_dir,"PAPA_K1.sav"))))
  
  labels = c(K11_2_1 = "Delayed or reduced speach",
             K11_2_2 = "Stereotyic and repetetive speach",
             K11_2_3 = "Shortcoming in the use of nonverbal behavior",
             K11_2_4 = "Lack of social or emotional reciprocity",
             K11_2_5 = "Weak ability to initiate or maintain a conversation",
             K11_2_6 = "Lack of spontaneous seeking to share enjoyment, interests and coping",
             K11_5_1 = "Lack of varied, spontaneous as-about play or social imitative play",
             K11_6_1 = "over-preoccupation with parts of objects",
             K11_7_1 = "Comprehensive preoccupation with stereotyped and narrow interests",
             K11_12_1 = "Weak ability to form friendships with peers",
             K11_14_1 = "Compulsive concerned specific, nonfunctional routines or rituals",
             K11_15_1 = "Stereotyped and repetitive motion mannerisms")
  
  domain = c("COMM","COMM","SOC","SOC","COMM","SOC","COMM","REP","REP","SOC","REP","REP")
  CSR = CSR[,c(index_vars,names(labels)),with = F]
  
  CSR = make_sum_scores(CSR,names(labels)[domain == "COMM"],"CSR.COMM.SS")
  CSR = make_sum_scores(CSR,names(labels)[domain == "REP"],"CSR.REP.SS")
  CSR = make_sum_scores(CSR,names(labels)[domain == "SOC"],"CSR.SOC.SS")
  
  labels[domain == "COMM"] = paste0("Papa; communication; ", labels[domain == "COMM"])
  labels[domain == "SOC"] = paste0("Papa; social play; ", labels[domain == "SOC"])
  labels[domain == "REP"] = paste0("Papa; repetetiv behavior; ", labels[domain == "REP"])
  
  for (v in names(labels)) attributes(CSR[[v]])$label = labels[v]
  
  setnames(CSR,names(labels),
           paste0(paste0("CSR.",domain,".i"),
                  cumsum(domain == "COMM")*(domain == "COMM") + 
                    cumsum(domain == "REP")*(domain == "REP") + 
                    cumsum(domain == "SOC")*(domain == "SOC")))
  
  ############################# merge and ADHD COMORBIDITIES ##############################
  
  PAPA = merge(AD, BH, by = c(index_vars), all = T)
  PAPA = merge(PAPA, AX, by = c(index_vars), all = T)
  PAPA = merge(PAPA, SL, by = c(index_vars), all = T)
  PAPA = merge(PAPA, KK, by = c(index_vars), all = T)
  PAPA = merge(PAPA, CSR, by = c(index_vars), all = T)
  PAPA = merge(PAPA, REG, by = c(index_vars), all = T)
  
  PAPA[,DIAG.GR := 4]
  PAPA[ADHD.CAT <= 2 & DBD.GR == 4 & ANX.GR == 4, DIAG.GR := 1]
  PAPA[DIAG.GR > 1 & ADHD.CAT <= 2 & (DBD.GR < 4 | ANX.GR < 4), DIAG.GR := 2]
  PAPA[DIAG.GR > 2 & ADHD.CAT == 3 & (DBD.GR < 4 | ANX.GR < 4), DIAG.GR := 3]
  PAPA[DIAG.GR > 3 & ADHD.CAT == 3 & DBD.GR == 4 & ANX.GR == 4, DIAG.GR := 4]
  my_labels = c("ADHD_only" = 1, "ADHD_w_COMROB" = 2, "OTHER_only" = 3, "no_Diagnosis" = 4)
  PAPA[,DIAG.GR := labelled(DIAG.GR,labels = my_labels)]
  
  setnames(PAPA,setdiff(names(PAPA),index_vars),
           paste0("PP.",setdiff(names(PAPA),index_vars)))
  
  ############################### Add labels to variable names ############################
  
  attributes(PAPA$PP.ADHD.SC) = list(label = "PAPA: Total number of present ADHD SYMPTOMs")
  attributes(PAPA$PP.ADHD.AT.SC) = list(label = "PAPA: Number of present inattentiveness SYMPTOMs")
  attributes(PAPA$PP.ADHD.HY.SC) = list(label = "PAPA: Number of present hyperactivity SYMPTOMs")
  attributes(PAPA$PP.ADHD.IM.SC) = list(label = "PAPA: Number of present impulsivity SYMPTOMs")
  attributes(PAPA$PP.ADHD.HI.SC) = list(label = "PAPA: Number of present hyperactivity & impulsiveness SYMPTOMs")
  
  abbreviations = c(PP = "PAPA",
                    SEA = "separation anxiety", 
                    IMP = "impairments", 
                    IMPstr = "strong impairments", 
                    IMPwk = "weak impairments", 
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
                    SOC = "social play",
                    COMM = "communication",
                    ER = "Emotion regulation",
                    SR = "Sensoric reactivity",
                    EH = "Eating habits",
                    REP = "repetetive behavior",
                    SC = "count of non-zero scores",
                    MI = "number of missing values",
                    GR = "diagnostic groups",
                    SG = "diagnostic sub groups",
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
                    go2bed = "When does the child go to bed? (minutes after 00:00)",
                    get_up = "When does the child usually get up in the morning? (minutes after 00:00)",
                    leaves_bed = "Leaves bed before going to sleep",
                    EMO = "affective disorder",
                    somnabul = "Somnabulisme",
                    XOTHER = "Other disorder than ADHD",
                    res_slp = "resistance to go to sleep",
                    h_night = "hours sleep per night",
                    CSR = "Communication, social behavior, and regulation",
                    DIF = "Conclusion: Difficulties",
                    DIA = "Conclusion: Diagnosis",
                    diag_labels,
                    C = "combined type",
                    A = "inattentive type")
  
  #   for (variable in names(PAPA)[-c(1,2)]){
  #     variable_info = strsplit(variable,split = "\\.")[[1]]
  #     if (length(translate[variable_info]) == length(variable_info)){
  #       label = paste0("", paste(translate[variable_info],collapse = "; "))
  #       if ( is(PAPA[[variable]],"labelled") ) {
  #         attributes(PAPA[[variable]])[["label"]] = label
  #       } else {
  #         attributes(PAPA[[variable]]) = list(label = label)
  #       }
  #     }
  #   }
  PAPA = add_label(PAPA,"PP",abbreviations)
  return(PAPA)
}
