

# all tests in here?
# https://www.duo.uio.no/bitstream/handle/10852/17905/X.pdf?sequence=1

#####################################################
######################## nepsy ######################
#####################################################
# http://books.google.no/books/about/Essentials_of_NEPSY_Assessment.html?id=PRxWMaVeVX4C&redir_esc=y

################ QUESTIONS ##############
# what is with NA values?
get_neuropsych = function(data_dir) {
  dt = NaN2NA(data.table(read_sav(paste0(data_dir,"NpY.sav"))))
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
  
  #tmp = NaN2NA(data.table(read_sav(paste0(data_dir,"Nepsy_Delscore.sav"))))
  #setnames(tmp,names(tmp)[1:2],names(dt)[1:2])
  #dt = merge(dt,tmp,by = c(index_vars), all = T)
  #setnames(dt,names(dt),gsub("ny","NY",names(dt)))
  #rm(tmp)
  
  #vnames = names(dt)
  #dt = make_sum_scores(dt,vnames[grep("NY2_1",vnames)], "NY.L.UI.1_13.S", count_score = F)
  #dt = make_sum_scores(dt,vnames[grep("NY3_2",vnames)], "NY.L.UI.14_20.S", count_score = F)
  #dt[,NY.L.UI.S := NY.L.UI.1_13.S + NY.L.UI.14_20.S]
  
  dt = dt[,-grep("NY3_|N3_",names(dt)),with = F]
  # remove scales that are not used
  dt = dt[,-grep("NY2_|NY7_|NEPSY8_1|NY1_2",names(dt)),with = F]
  # remove variables with comments (strings) and other stuff
  dt = dt[,-grep("NY1_2|NY4_3|NY6_2|NYINSTRUMENT_ID|NYSTATUS|VERSJON_NPY_TBL1",names(dt)),with = F]
  
    
  # Visuospatial Processing Domain
  setnames(dt,"NY1_1","NY.VSP.DesCop.S")
  # Attention and Executive Functioning
  setnames(dt,"NY6_1","NY.INHIB.Statue.S")
  # Language
  setnames(dt,"NY4_2","NY.LANG.PhonProc.S")
  # Visual Attention
  dt[,NY.VA.cats.S := char2num(dt$NY5_2_3)]
  dt[NY.VA.cats.S > 20, NY.VA.cats.S:=NA]
  print("set NY.VA.cats.S values larger 20 set to NA")
  dt[,NY.VA.cats.T := char2num(NY5_2_4)]
  dt[,NY.VA.bunniescats.S := char2num(dt$NY5_3_3)]
  dt[NY.VA.bunniescats.S > 45, NY.VA.bunniescats.S := NA]
  print("set NY.VA.bunniescats.S values larger 45 set to NA")
  setnames(dt,"NY5_3_4","NY.VA.bunniescats.T")
  dt[, NY.VA.bunniescats.T := char2num(NY.VA.bunniescats.T)]
  
  dt = dt[,-grep("NY5_",names(dt)),with = F]
  
  abbreviations = c(NY = "NEPSY",
                    LANG = "Language",
                    VA = "Visual attention",
                    VSP = "Visuospatial processing",
                    DesCop = "design copying",
                    S = "Score",
                    T = "time",
                    UI = "understanding instructions",
                    PhonProc = "phonological processing",
                    INHIB = "Inhibition")
  dt = add_label(dt,"NY.",abbreviations,my_warning = F)
  
  #####################################################
  ############## Boston naming task ##################
  #####################################################
  bnt = NaN2NA(data.table(read_sav(paste0(data_dir,"BNT.sav"))))
  SDcols = names(bnt)[grep("BN1_",names(bnt))]
  
  bnt[,BNT.compl := is.na(BNBNT0)]
  bnt[BN2_1 == "Ikke tatt" | BN2_1 == "3 , men avbrutt" , BNT.compl := F]
  
  bnt[,BNT.miss := sum(is.na(.SD)), by = 1:nrow(bnt),.SDcols = SDcols]
  
  bnt = smart_impute(bnt,items = names(bnt) %in% SDcols)
  
  bnt[,BNT.S := sum(.SD<5,na.rm = T), by = 1:nrow(bnt),.SDcols = SDcols]
  bnt[BNT.compl == F,BNT.S := NA]
  bnt[,BNT.errors := sum(.SD == 5, na.rm = T), by = 1:nrow(bnt),.SDcols = SDcols]
  
  bnt[BNT.miss == 25, BNT.compl := F]
  bnt[BNT.errors < 5 & BNT.S < 10,BNT.compl := F]
  bnt[BNT.compl != T,BNT.S := NA]
  
  abbreviations = c(BNT = "Boston naming task",
                    compl = "number of completed items",
                    miss = "number of missing items",
                    errors = "number of errors",
                    S = "Score")
  setnames(bnt,SDcols,paste0("BNT.i",sprintf("%02d",1:25)))
  
  bnt = bnt[,c(index_vars,names(bnt)[grep("^BNT",names(bnt))]),with = F]
  bnt = add_label(bnt,"BNT",abbreviations,my_warning = F)
  
  dt = merge(dt,bnt,by = c(index_vars), all = T)
  
  rm(bnt)
  
  
  #####################################################
  ################ COOKIE DELAY TASK ##################
  #####################################################
  cdt = NaN2NA(data.table(read_sav(paste0(data_dir,"CDT.sav"))))
  
  invalids = grep("Usikkert om hun forsto|Far holdt en|Ikke skårbar|ikke valid|spiste annenkjeks under opg|Ikke valid|Mor gir tegn til at barnet|Tror hun forstår instruksjon|Får hjelp av mor|Usikker på om hun forstår|Oppg. avlsuttet pga manglende samarbeid",
                  cdt[,CD1_3])
  cdt[invalids,CD1_2 := NA]
  setnames(cdt,"CD1_2","CDT.S")
  attributes(cdt[["CDT.S"]])$label = "Score cookie delay task"
  dt = merge(dt,cdt[,c(index_vars,"CDT.S"),with = F],by = c(index_vars), all = T)
  rm(cdt)
  
  #####################################################
  ############# TRUCK REVERSAL LEARNING ###############
  #####################################################
  trl = NaN2NA(data.table(read_sav(paste0(data_dir,"TRLT.sav"))))
  
  setnames(trl,paste0("TR1_1_",2:13),paste0("TT.A.i",1:12))
  setnames(trl,paste0("TR2_1_",1:8),paste0("TT.B.i",1:8))
  
  setnames(trl,"TR1_2_1","TT.A.lrd")
  setnames(trl,"TR1_3","TT.A.errors")
  setnames(trl,"TR1_4","TT.A.t2c")
  setnames(trl,"TR2_2_1","TT.B.lrd")
  setnames(trl,"TR2_3","TT.B.errors")
  setnames(trl,"TR2_4","TT.B.t2c")
  trl[,TT.B.t2c := char2num(TT.B.t2c)]
  
  abbreviations = c(TT = "Truck reversal learning task",
                    A = "pre reversal/switch",
                    B = "post reversal/switch",
                    errors = "number of errors",
                    t2c = "number trials to criterion",
                    lrd = "learned")
  
  trl = trl[,-grep("^TR|^VERS",names(trl)),with = F]
  
  trl = add_label(trl,"TT",abbreviations,my_warning = F)
  
  dt = merge(dt,trl,by = c(index_vars), all = T)
  rm(trl)
  
  
  #####################################################
  ################## Spin the Pots ####################
  #####################################################
  # https://paperpile.com/view/49565dba-6695-029a-ac69-7e1d9bd95028
  # Hughes & ENsor 2005
  
  stp = NaN2NA(data.table(read_sav(paste0(data_dir,"SnurrB.sav"))))
  setnames(stp,"SNURR1_1","STP.t2c")
  setnames(stp,"SNURR1_2","STP.err_e")
  setnames(stp,"SB1_3","STP.err_f")
  setnames(stp,"SNURR1_4","STP.err_t")
  setnames(stp,"SNURR1_5","STP.S")
  setnames(stp,"SNURR1_8","STP.io")
  
  stp = stp[, c(which(index_vars %in% names(stp)),
                grep("^STP",names(stp))),with = F]
  
  abbreviations = c(STP = "Spin the pots task",
                    err_e = "number of errors empty boxes",
                    err_f = "number of errors boxes with sticker",
                    err_t = "total number of errors",
                    t2c = "number trials to criterion",
                    S = "Score",
                    io = "impulsive openings")
  
  stp = add_label(stp,"STP",abbreviations,my_warning = F)
  
  dt = merge(dt,stp,by = c(index_vars), all = T)
  rm(stp)
  #####################################################
  ################## Grooved Pegboard #################
  #####################################################
  # http://www.si-instruments.com/supplier/files/download/lafayette-current-version-grooved-pegboard-test-32025-lafayette-32025-grooved-pegboard-test-manual-pdf.html
  # psychomotoric spees
  
  gpt = NaN2NA(data.table(read_sav(paste0(data_dir,"Pegs.sav"))))
  gpt[GP1 == "NaN",GP1 := NA]
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
  
  gpt = gpt[, c(which(index_vars %in% names(gpt)),
                grep("^GPT",names(gpt))),with = F]
  
  abbreviations = c(GPT = "Grooved pegboard task",
                    dh = "dominant hand",
                    d = "dominant hand",
                    nd = "non-dominant hand",
                    n_miss = "number missed pins",
                    n2h = "number used two hands",
                    sec = "seconds")
  for (n in names(gpt)[-c(1,2)]) gpt[[n]] = as.numeric(gpt[[n]])
  gpt = add_label(gpt,"GPT",abbreviations)
  dt = merge(dt,gpt,by = c(index_vars), all = T)
  rm(gpt)
  return(dt)
}

