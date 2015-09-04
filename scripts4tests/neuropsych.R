

# all tests in here?
# https://www.duo.uio.no/bitstream/handle/10852/17905/X.pdf?sequence=1

#####################################################
######################## nepsy ######################
#####################################################
# http://books.google.no/books/about/Essentials_of_NEPSY_Assessment.html?id=PRxWMaVeVX4C&redir_esc=y

################ QUESTIONS ##############
# what is with NA values?
get_neuropsych = function() {
  dt = data.table(read_sav("F:/Forskningsprosjekter/PDB 299 - ADHD-studien Prescho_/Forskningsfiler/GUBI/GuidoData/masterfile/savs/NpY.sav"))
  #dt = data.table(dt[,-c(27:29,32,33)])
  
  ########### corrections Nepsy from Nina ####################
  #Forståelse av instruksjoner: summere opp NY3_3 og NY3_4 = N3_5ny for å luke ut regnefeil.
  #I tillegg er det flere kommentarer som kvalifiserer for å sette skårene til missing, men der testleder på undersøkelsesdagen allerede satte skårene til missing.
  
  #NY3_3+NY3_4=N3_5ny, men bare hvis ikke 3_4 er missing. Hvis den er missing blir sumskåren =NY3_3.
  dt[,N3_5 := NY3_3 + NY3_4] # note: NY3_3 is only missing in cases where NY3_4 is also missing
  #Skårer med følgende kommentarer settes til missing:
  #"trett, tøyser, ikke valid", "Test ikke gjennomført. Barnet ville ikke. Ikke valid.".
  invalid_scores = c(which(dt$NY3_6 == "trett, tøyser, ikke valid" |
                           dt$NY3_6 == "Test ikke gjennomført. Barnet ville ikke. Ikke valid."),
                     grep("serbisk",dt[,NY1_2]))
  dt[invalid_scores,N3_5 := NA]
  
  tmp = data.table(read_sav("F:/Forskningsprosjekter/PDB 299 - ADHD-studien Prescho_/Forskningsfiler/GUBI/GuidoData/masterfile/savs/Nepsy_Delscore.sav"))
  setnames(tmp,names(tmp)[1:2],names(dt)[1:2])
  dt = merge(dt,tmp,by = c("PREG_ID_299","BARN_NR"))
  setnames(dt,names(dt),gsub("ny","NY",names(dt)))
  rm(tmp)
  
  vnames = names(dt)
  dt[,NY.undrst.sum1 := sum(.SD), by = c("PREG_ID_299","BARN_NR"), .SDcols = vnames[grep("NY2_1",vnames)]]
  dt[,NY.undrst.sum2 := sum(.SD), by = c("PREG_ID_299","BARN_NR"), .SDcols = vnames[grep("NY3_2",vnames)]]
  dt[,NY.undrst.S := NY.undrst.sum1 + NY.undrst.sum2,by = c("PREG_ID_299","BARN_NR")]
  
  # Visuospatial Processing Domain
  setnames(dt,"NY1_1","NY.VISPROC.DesignCopying.S")
  # Attention and Executive Functioning
  setnames(dt,"NY6_1","NY.INHIB.Statue.S")
  # Language
  setnames(dt,"NY.undrst.S","NY.L.ComprehInstr.S")
  setnames(dt,"NY4_2","NY.L.PhonProc")
  # Visual Attention
  dt[,NY.VI.cats.S := as.numeric(dt$NY5_2_3)]
  dt[NY.VI.cats.S > 20, NY.VI.cats.S:=20]
  print("2 NY.VI.cats.S values larger 20 set to 20")
  dt[,NY.VI.cats.T := as.numeric(gsub("[a-z.]","",NY5_2_4))]
  dt[,NY.VI.bunniescats.S := as.numeric(dt$NY5_3_3)]
  dt[NY.VI.bunniescats.S > 45, NY.VI.bunniescats.S := NA]
  setnames(dt,"NY5_3_4","NY.VI.bunniescats.T")
  
  setnames(dt,names(dt))

  abbreviations = c(NY = "NEPSY (Developmental Neuropsychological Assessment)",
                    L = "Language",
                    VI = "Visual attention",
                    S = "Score",
                    T = "time",
                    undrst = "understanding")
  
  #####################################################
  ############## Boston naming task ##################
  #####################################################
  bnt = data.table(read_sav("F:/Forskningsprosjekter/PDB 299 - ADHD-studien Prescho_/Forskningsfiler/GUBI/GuidoData/masterfile/savs/BNT.sav"))
  SDcols = names(bnt)[grep("BN1_",names(bnt))]
  
  bnt[,BNT.compl := is.na(BNBNT0)]
  bnt[BN2_1 == "Ikke tatt" | BN2_1 == "3 , men avbrutt" , BNT.compl := F]
  
  bnt[BNT.compl == T,BNT.S := sum(.SD<5,na.rm = T),by = list(PREG_ID_299,BARN_NR),.SDcols = SDcols]
  bnt[,BNT.misss := sum(is.na(.SD)),by = list(PREG_ID_299,BARN_NR),.SDcols = SDcols]
  bnt[,BNT.errors := sum(.SD == 5, na.rm = T),by = list(PREG_ID_299,BARN_NR),.SDcols = SDcols]
  
  bnt[BNT.misss == 25, BNT.compl := F]
  bnt[BNT.errors < 5 & BNT.S < 10,BNT.compl := F]
  bnt[BNT.compl != T,BNT.S := NA]
  
  abbreviations = c(BNT = "Boston naming task",
                    compl = "number of completed items",
                    miss = "number of missing items",
                    errors = "number of errors",
                    S = "Score")
  
  dt = merge(dt,bnt,by = c("PREG_ID_299","BARN_NR"))
  
  rm(bnt)
  
  
  #####################################################
  ################ COOKIE DELAY TASK ##################
  #####################################################
  cdt = data.table(read_sav("F:/Forskningsprosjekter/PDB 299 - ADHD-studien Prescho_/Forskningsfiler/GUBI/GuidoData/masterfile/savs/CDT.sav"))
  setnames(cdt,"CD1_2","CDT.SS")
  attributes(cdt[["CDT.SS"]])$label = "Sumscore cookie delay task"
  dt = merge(dt,cdt,by = c("PREG_ID_299","BARN_NR"))
  rm(cdt)
  
  #####################################################
  ############# TRUCK REVERSAL LEARNING ###############
  #####################################################
  trl = data.table(read_sav("F:/Forskningsprosjekter/PDB 299 - ADHD-studien Prescho_/Forskningsfiler/GUBI/GuidoData/masterfile/savs/TRLT.sav"))
  setnames(trl,"TR1_2_1","TRLT.A.lrd")
  setnames(trl,"TR1_3","TRLT.A.errors")
  setnames(trl,"TR1_4","TRLT.A.t2c")
  setnames(trl,"TR2_2_1","TRLT.B.lrd")
  setnames(trl,"TR2_3","TRLT.B.errors")
  setnames(trl,"TR2_4","TRLT.B.t2c")
  
  abbreviations = c(TRLT = "Truck reversal learning task",
                    errors = "number of errors",
                    t2c = "number trials to criterion",
                    lrd = "learned")
  
  dt = merge(dt,trl,by = c("PREG_ID_299","BARN_NR"))
  rm(trl)
  
  
  #####################################################
  ################## Spin the Pots ####################
  #####################################################
  # https://paperpile.com/view/49565dba-6695-029a-ac69-7e1d9bd95028
  # Hughes & ENsor 2005
  
  stp = data.table(read_sav("F:/Forskningsprosjekter/PDB 299 - ADHD-studien Prescho_/Forskningsfiler/GUBI/GuidoData/masterfile/savs/SnurrB.sav"))
  setnames(stp,"SNURR1_1","STP.t2c")
  setnames(stp,"SNURR1_2","STP.err_e")
  setnames(stp,"SB1_3","STP.err_f")
  setnames(stp,"SNURR1_4","STP.err_t")
  setnames(stp,"SNURR1_5","STP.S")
  setnames(stp,"SNURR1_8","STP.io")
  abbreviations = c(STP = "spin the pots task",
                    err_e = "number of errors empty",
                    err_f = "number of errors full",
                    err_t = "total number of errors",
                    t2c = "number trials to criterion",
                    S = "Score",
                    io = "impulsive openings")
  dt = merge(dt,stp,by = c("PREG_ID_299","BARN_NR"))
  rm(stp)
  #####################################################
  ################## Grooved Pegboard #################
  #####################################################
  # http://www.si-instruments.com/supplier/files/download/lafayette-current-version-grooved-pegboard-test-32025-lafayette-32025-grooved-pegboard-test-manual-pdf.html
  # psychomotoric spees
  
  gpt = data.table(read_sav("F:/Forskningsprosjekter/PDB 299 - ADHD-studien Prescho_/Forskningsfiler/GUBI/GuidoData/masterfile/savs/Pegs.sav"))
  gpt[,GP1 := factor(GP1,labels = c("right","left"))]
  setnames(gpt,"GP1","GPT.dh")
  setnames(gpt,"GP2_1","GPT.d.sec")
  setnames(gpt,"GP2_2","GPT.d.n_miss")
  setnames(gpt,"GP2_3","GPT.d.n2h")
  setnames(gpt,"GP3_1","GPT.nd.sec")
  setnames(gpt,"GP3_2","GPT.nd.n_miss")
  setnames(gpt,"GP3_3","GPT.nd.n2h")
  gpt$GPT.d.sec = char2num(gpt$GPT.d.sec)
  gpt$GPT.nd.sec = char2num(gpt$GPT.nd.sec)
  
  abbreviations = c(GPT = "Grooved pegboard task",
                    dh = "dominant hand",
                    d = "dominant hand",
                    nd = "non-dominant hand",
                    n_miss = "number misses",
                    n2h = "number to hand",
                    sec = "seconds")
  gpt = add_label(gpt,"GPT",abbreviations)
  dt = merge(dt,gpt,by = c("PREG_ID_299","BARN_NR"))
  rm(gpt)
  return(dt)
}