

# all tests in here?
# https://www.duo.uio.no/bitstream/handle/10852/17905/X.pdf?sequence=1

#####################################################
######################## nepsy ######################
#####################################################
# http://books.google.no/books/about/Essentials_of_NEPSY_Assessment.html?id=PRxWMaVeVX4C&redir_esc=y

################ QUESTIONS ##############
# what is with NA values?
get_neuropsych = function() {
  dt = data.table(read_sav("savs/NpY.sav"))
  #dt = data.table(dt[,-c(27:29,32,33)])
  
  ########### corrections Nepsy from Nina ####################
  #Forståelse av instruksjoner: summere opp NY3_3 og NY3_4 = N3_5ny for å luke ut regnefeil.
  #I tillegg er det flere kommentarer som kvalifiserer for å sette skårene til missing, men der testleder på undersøkelsesdagen allerede satte skårene til missing.
  
  #NY3_3+NY3_4=N3_5ny, men bare hvis ikke 3_4 er missing. Hvis den er missing blir sumskåren =NY3_3.
  dt[,N3_5 := NY3_3 + NY3_4] # note: NY3_3 is only missing in cases where NY3_4 is also missing
  #Skårer med følgende kommentarer settes til missing:
  #"trett, tøyser, ikke valid", "Test ikke gjennomført. Barnet ville ikke. Ikke valid.".
  invalid_scores = which(dt$NY3_6 == "trett, tøyser, ikke valid" | dt$NY3_6 == "Test ikke gjennomført. Barnet ville ikke. Ikke valid.")
  dt[invalid_scores,N3_5 := NA]
  
  tmp = data.table(read_sav("savs/Nepsy_Delscore.sav"))
  setnames(tmp,names(tmp)[1:2],names(dt)[1:2])
  dt = merge(dt,tmp,by = c("PREG_ID_299","BARN_NR"))
  setnames(dt,names(dt),gsub("ny","NY",names(dt)))
  rm(tmp)
  
  vnames = names(dt)
  dt[,NEPSY.understanding_sum1 := sum(.SD), by = c("PREG_ID_299","BARN_NR"), .SDcols = vnames[grep("NY2_1",vnames)]]
  dt[,NEPSY.understanding_sum2 := sum(.SD), by = c("PREG_ID_299","BARN_NR"), .SDcols = vnames[grep("NY3_2",vnames)]]
  dt[,NEPSY.understanding.SCORE := NEPSY.understanding_sum1 + NEPSY.understanding_sum2,by = c("PREG_ID_299","BARN_NR")]
  
  # Visuospatial Processing Domain
  setnames(dt,"NY1_1","NEPSY.VISPROC.DesignCopying.SCORE")
  # Attention and Executive Functioning
  setnames(dt,"NY6_1","NEPSY.INHIB.Statue.SCORE")
  # Language
  setnames(dt,"NEPSY.understanding.SCORE","NEPSY.LANG.ComprehInstr.SCORE")
  setnames(dt,"NY4_2","NEPSY.LANG.PhonProc")
  # Visual Attention
  dt[,NEPSY.VISATT.cats.SCORE := as.numeric(dt$NY5_2_3)]
  dt[,NEPSY.VISATT.cats_time := as.numeric(gsub("[a-z.]","",NY5_2_4))]
  dt[,NEPSY.VISATT.bunniescats.SCORE := as.numeric(dt$NY5_3_3)]
  dt[NEPSY.VISATT.bunniescats.SCORE > 45, NEPSY.VISATT.bunniescats.SCORE := NA]
  setnames(dt,"NY5_3_4","NEPSY.VISATT.bunniescats_time")
  
  #####################################################
  ############## Boston naming task ##################
  #####################################################
  bnt = data.table(read_sav("savs/BNT.sav"))
  SDcols = names(bnt)[grep("BN1_",names(bnt))]
  
  bnt[,BNT.completed := is.na(BNBNT0)]
  bnt[BN2_1 == "Ikke tatt" | BN2_1 == "3 , men avbrutt" , BNT.completed := F]
  
  bnt[BNT.completed == T,BNT.SCORE := sum(.SD<5,na.rm = T),by = list(PREG_ID_299,BARN_NR),.SDcols = SDcols]
  bnt[,BNT.missings := sum(is.na(.SD)),by = list(PREG_ID_299,BARN_NR),.SDcols = SDcols]
  bnt[,BNT.errors := sum(.SD == 5, na.rm = T),by = list(PREG_ID_299,BARN_NR),.SDcols = SDcols]
  
  bnt[BNT.missings == 25, BNT.completed := F]
  bnt[BNT.errors < 5 & BNT.SCORE < 10,BNT.completed := F]
  bnt[BNT.completed != T,BNT.SCORE := NA]
  
  dt = merge(dt,bnt,by = c("PREG_ID_299","BARN_NR"))
  rm(bnt)
  
  
  #####################################################
  ################ COOKIE DELAY TASK ##################
  #####################################################
  cdt = data.table(read_sav("savs/CDT.sav"))
  setnames(cdt,"CD1_2","CDT.sum.SCORE")
  dt = merge(dt,cdt,by = c("PREG_ID_299","BARN_NR"))
  rm(cdt)
  
  #####################################################
  ############# TRUCK REVERSAL LEARNING ###############
  #####################################################
  trl = data.table(read_sav("savs/TRLT.sav"))
  setnames(trl,"TR1_2_1","TRLT.A.learned")
  setnames(trl,"TR1_3","TRLT.A.numbererrors")
  setnames(trl,"TR1_4","TRLT.A.trials2crit")
  setnames(trl,"TR2_2_1","TRLT.B.learned")
  setnames(trl,"TR2_3","TRLT.B.numbererrors")
  setnames(trl,"TR2_4","TRLT.B.trials2crit")
  dt = merge(dt,trl,by = c("PREG_ID_299","BARN_NR"))
  rm(trl)
  
  
  #####################################################
  ################## Spin the Pots ####################
  #####################################################
  # https://paperpile.com/view/49565dba-6695-029a-ac69-7e1d9bd95028
  # Hughes & ENsor 2005
  
  stp = data.table(read_sav("savs/SnurrB.sav"))
  setnames(stp,"SNURR1_1","STP.trials2crit")
  setnames(stp,"SNURR1_2","STP.numerrorsempty")
  setnames(stp,"SB1_3","STP.numerrorsfull")
  setnames(stp,"SNURR1_4","STP.totalerrors")
  setnames(stp,"SNURR1_5","STP.SCORE")
  setnames(stp,"SNURR1_8","STP.impulsopenings")
  dt = merge(dt,stp,by = c("PREG_ID_299","BARN_NR"))
  rm(stp)
  
  
  #####################################################
  ################## Grooved Pegboard #################
  #####################################################
  # http://www.si-instruments.com/supplier/files/download/lafayette-current-version-grooved-pegboard-test-32025-lafayette-32025-grooved-pegboard-test-manual-pdf.html
  # psychomotoric spees
  
  gpt = data.table(read_sav("savs/Pegs.sav"))
  gpt[,GP1 := factor(GP1,labels = c("right","left"))]
  setnames(gpt,"GP1","GPT.dominanthand")
  setnames(gpt,"GP2_1","GPT.dominant.seconds")
  setnames(gpt,"GP2_2","GPT.dominant.numbermissed")
  setnames(gpt,"GP2_3","GPT.dominant.number2hands")
  setnames(gpt,"GP3_1","GPT.ndomnt.seconds")
  setnames(gpt,"GP3_2","GPT.ndomnt.numbermissed")
  setnames(gpt,"GP3_3","GPT.ndomnt.number2hands")
  
  dt = merge(dt,gpt,by = c("PREG_ID_299","BARN_NR"))
  rm(gpt)
  return(dt)
}