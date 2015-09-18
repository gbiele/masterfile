
get_StanfordBinet = function(){
  
  StBdata <- read_sav("savs/StB.sav");
  agedata <- read_sav("savs/ADHD_Score.sav");
  agedata[,"Gender"] = gsub("X_","",agedata[,"Gender"])
  names(agedata)[names(agedata) == "barn_nr"] = "BARN_NR"
  StBdata = merge(StBdata,agedata[,c(index_vars,"Kontroll_Alder","Gender")],by = c(index_vars),all.x = T, all.y = F)
  rm(agedata)
  
  StBdata = data.table(StBdata)
  
  for (v in index_vars) StBdata[, c(v) := as.numeric(get(v))]
  
  StBdata[, Age_in_months := Kontroll_Alder/365.25*12]
  
  ###### data cleaning following Nina's SPSS script ###############
  #Nå lager jeg først en ny variabel som er lik SB3_3 men der alle missing er satt til 0.
  #Grunnen for å gjøre dette er fordi det er ekstremt mye missing her, uten at det ser ut til å være en grunn til det i kommentarene. 
  #Jeg antar derfor at de aller, aller fleste missing på SB3_3 egentlig er 0-ere. Denne hypotesen støttes av at det ikke finnes en eneste 0-skåre i SB3_3!
  StBdata[is.na(SB3_3), SB3_3 := 0]
  non_valid_scores = which(StBdata$SB3_5 == "Ikke valid. Nr. 2 og 3 ikke administrert. Avbrøt også tidlig. Barnet vegret." |
                           StBdata$SB3_5 == "Serbisk, mor oversetter men nr 21,23 ikke oversettbar")
  
  #################### NVIQ, Non-verbal routing ##################
  StBdata[Age_in_months >= 37 & Age_in_months < 42, NVIQ := mapvalues(SB1_2,0:16,3:19,warn_missing = F)]
  StBdata[Age_in_months >= 42 & Age_in_months < 48, NVIQ := mapvalues(SB1_2,0:17,c(2:7,9,10,10,11:19),warn_missing = F)]
  
  #################### VIQ Verbal IQ ##################
  StBdata[,SB3_4 := SB3_2 + SB3_3]
  StBdata[non_valid_scores, SB3_4 := NA]
  StBdata[Age_in_months < 38              , VIQ := mapvalues(SB3_4,0:26,sort(c(1:19,1,2,5,8,12,15,18,18)),warn_missing = F)]
  StBdata[Age_in_months >= 38 & Age_in_months < 40, VIQ := mapvalues(SB3_4,0:26,c(1,1,1,2,2,3,4,5,5,6,7,8,8,9,10,11,11,12,13,14,14,15,16,17,17,18,19),warn_missing = F)]
  StBdata[Age_in_months >= 40 & Age_in_months < 42, VIQ := mapvalues(SB3_4,0:27,c(1,1,1,2,2,3,4,5,5,6,7,7,8,9,10,10,11,12,12,13,14,15,15,16,17,17,18,19),warn_missing = F)]
  StBdata[Age_in_months >= 42 & Age_in_months < 44, VIQ := mapvalues(SB3_4,0:28,c(rep(1,4),2,2,3,4,4,5,6,7,7,8,9,10,10,11,12,13,13,14,15,16,16,17,18,18,19),warn_missing = F)]
  StBdata[Age_in_months >= 44 & Age_in_months < 46, VIQ := mapvalues(SB3_4,0:29,c(rep(1,5),2,3,4,4,5,6,6,7,8,9,9,10,11,11,12,13,14,14,15,16,16,17,18,18,19),warn_missing = F)]
  StBdata[Age_in_months >= 46 & Age_in_months < 48, VIQ := mapvalues(SB3_4,0:30,c(rep(1,5),2,2,3,4,4,5,6,7,7,8,9,9,10,11,11,12,13,14,14,15,16,16,17,18,18,19),warn_missing = F)]
  
  
  #################### NVWMS Nonverbal Working Memory  ##################
  StBdata[Age_in_months < 38 , NVWMS := mapvalues(SB5_5,0:15,c(2,3,5,6,8:19),warn_missing = F)]
  StBdata[Age_in_months >= 38 & Age_in_months < 40, NVWMS := mapvalues(SB5_5,0:15,c(2:3,5:7,9:19),warn_missing = F)]
  StBdata[Age_in_months >= 40 & Age_in_months < 42, NVWMS := mapvalues(SB5_5,0:15,c(2:3,5:13,15:19),warn_missing = F)]
  StBdata[Age_in_months >= 42 & Age_in_months < 44, NVWMS := mapvalues(SB5_5,0:16,c(1,2,4:6,8:19),warn_missing = F)]
  StBdata[Age_in_months >= 44 & Age_in_months < 46, NVWMS := mapvalues(SB5_5,0:16,c(1,2,4:12,14:19),warn_missing = F)]
  StBdata[Age_in_months >= 46 & Age_in_months < 48, NVWMS := mapvalues(SB5_5,0:17,c(1:4,6:19),warn_missing = F)]
  
  #################### VWMS Verbal Working Memory  ##################
  StBdata[Age_in_months < 38, VWMS := mapvalues(SB7_5,0:15,c(3:6,8:9,11:17,18,18,19),warn_missing = F)]
  StBdata[Age_in_months >= 38 & Age_in_months < 40, VWMS := mapvalues(SB7_5,0:15,c(3:6,8:19),warn_missing = F)]
  StBdata[Age_in_months >= 40 & Age_in_months < 42, VWMS := mapvalues(SB7_5,0:15,c(3:15,17:19),warn_missing = F)]
  StBdata[Age_in_months >= 42 & Age_in_months < 44, VWMS := mapvalues(SB7_5,0:16,c(2:5,7:19),warn_missing = F)]
  StBdata[Age_in_months >= 44 & Age_in_months < 46, VWMS := mapvalues(SB7_5,0:16,c(1,2,4:12,14:19),warn_missing = F)]
  StBdata[Age_in_months >= 46 & Age_in_months < 48, VWMS := mapvalues(SB7_5,0:17,c(2:19),warn_missing = F)]
  
  StBdata[StBdata$SB3_5 == "Ikke valid. Nr. 2 og 3 ikke administrert. Avbrøt også tidlig. Barnet vegret.",VWMS := NA]
  
  #################### VOS Fluid Reasoning ####################
  StBdata[Age_in_months >= 37 & Age_in_months < 40, VWMS := mapvalues(SB6_5,0:11,c(3,5,7,9,10,12:14,16:19),warn_missing = F)]
  StBdata[Age_in_months >= 40 & Age_in_months < 42, VWMS := mapvalues(SB6_5,0:12,c(3,5,7,9,10,11,13:19),warn_missing = F)]
  StBdata[Age_in_months >= 42 & Age_in_months < 44, VWMS := mapvalues(SB6_5,0:13,c(3,5,7:9,11:13,15:17,18,18,19),warn_missing = F)]
  StBdata[Age_in_months >= 44 & Age_in_months < 46, VWMS := mapvalues(SB6_5,0:13,c(3,5,7:10,12:19),warn_missing = F)]
  StBdata[Age_in_months >= 46 & Age_in_months < 48, VWMS := mapvalues(SB6_5,0:13,c(3,5,7:15,17:19),warn_missing = F)]
  
  
  ################################ Abbreviated IQ (ABIQ) ################################
  
  # Based on sum NVIQ and VIQ. 
  # Sum of scaled scores, routing:
  # NB: In SPSS, NAs are treated as 0 when added to a value.
  # In the SPSS syntax, a new variable 'Valid_IQ_score' is computed to 
  # indicate subjects with no missings in either NVIQ or VIQ.
  # Here, subjects with one missing in NVIQ or VIQ are coded "NA" in sum_NV_V.
  
  # Alternatively: Recoding to resemble the variable generated in SPSS (if desireable...):
  # NVIQ[is.na(NVIQ)] <- 0
  # VIQ[is.na(VIQ)] <- 0
  # sum_NV_V <- NVIQ+VIQ
  
  sum_NV_V_val <- seq(2, 38)
  ABIQ_val <- c(47, 50, 52, seq(55, 148, by=3), 150, 153)
  PercRank_val <- c(rep(0.1, 4), 0.3, rep(1, 3), 2, 4, 5, 8, 12, 16, 
                    21, 27, 34, 42, 50, 58, 66, 73, 79, 84, 88, 92, 
                    95, 96, 98, 99, 99, 99.5, 99.7, rep(99.9, 4))
  
  StBdata[,ABIQ := mapvalues(NVIQ+VIQ,sum_NV_V_val,ABIQ_val,warn_missing = F)]
  StBdata[,ABIQ.PR := mapvalues(NVIQ+VIQ,sum_NV_V_val,PercRank_val,warn_missing = F)]
 
  invalid_scores = c(grep("oversatt",StBdata$SB5_6),grep("Avbrøt",StBdata$SB5_6))
  StBdata[invalid_scores, ABIQ := NA]
  StBdata[invalid_scores, ABIQ.PR := NA]
  
  ################################ Working memory (WM) index ################################
  # Based on sum NVWM and VWM.
  # From table SB-5 Examiner's Manual. Mean=100. 

  # Recoding to resemble the variable generated in SPSS (if desireable...):
  # NVWMS[is.na(NVWMS)] <- 0
  # VWMS[is.na(VWMS)] <- 0
  # WMsum = NVWMS + VWMS
  WMsum_val <- seq(2, 38)
  WMindex_val <- c(seq(48, 63, 3), 65, 
                   seq(68, 89, 3), 91,
                   seq(94, 115, 3), 117,
                   seq(120, 141, 3), 143,
                   seq(146, 152, 3))
  StBdata[,WMindex := mapvalues(NVWMS + VWMS,WMsum_val,WMindex_val,warn_missing = F)]
  
 
  #################### rename variables ###################
   
  setnames(StBdata,c("VERSJON_STB_TBL1", "SBINSTRUMENT_ID","Kontroll_Alder"),c("SB.Version","SB.Instrument_id","Age_in_days"))
  old_item_names = names(StBdata)[grep("SB[0-9]_",names(StBdata))]
  new_item_names = gsub("SB","SB.i",old_item_names)
  setnames(StBdata,old_item_names,new_item_names)
  
  old_names = c("NVIQ","VIQ","NVWMS","VWMS","ABIQ","ABIQ.PR","WMindex" )
  new_names = gsub("PR.S","PR",paste0("SB.",old_names,".S"))
  setnames(StBdata,old_names,new_names)

  abbreviations = c(SB = "Stanford Binet test",
                    S = "Score",
                    ABIQ = "Abbreviated IQ",
                    VIQ = "verbal IQ",
                    NVIQ = "non-verbal IQ",
                    VWMS = "visual working memory",
                    NVWMS = "non-visual working memory",
                    WMindex = "workig memory index",
                    PR = "percent rank")
  StBdata = StBdata[,c(1,2,grep("^SB\\.|Age|Gender",names(StBdata))),with = F]
  attributes(StBdata$Age_in_days) = list(label = "Age in days at data collection for ADHD Study")
  attributes(StBdata$Age_in_months) = list(label = "Age in months at data collection for ADHD Study")
  StBdata = add_label(StBdata,"SB",abbreviations,my_warning = F)
  return(StBdata)
}

