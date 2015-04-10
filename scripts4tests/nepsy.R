

# all tests in here?
# https://www.duo.uio.no/bitstream/handle/10852/17905/X.pdf?sequence=1

#####################################################
######################## nepsy ######################
#####################################################
# http://books.google.no/books/about/Essentials_of_NEPSY_Assessment.html?id=PRxWMaVeVX4C&redir_esc=y

################ QUESTIONS ##############
# what is with NA values?
get_neuropsych = function() {
  dt = read_sav("savs/NpY.sav")
  dt = data.table(dt[,-c(27:29,32,33)])
  tmp = data.table(read_sav("savs/Nepsy_Delscore.sav"))
  setnames(tmp,names(tmp)[1:2],names(dt)[1:2])
  dt = merge(dt,tmp,by = c("PREG_ID_299","BARN_NR"))
  setnames(dt,names(dt),gsub("ny","NY",names(dt)))
  rm(tmp)
  
  vnames = names(dt)
  dt[,nepsy_understanding_sum1 := sum(.SD), by = c("PREG_ID_299","BARN_NR"), .SDcols = vnames[grep("NY2_1",vnames)]]
  dt[,nepsy_understanding_sum2 := sum(.SD), by = c("PREG_ID_299","BARN_NR"), .SDcols = vnames[grep("NY3_2",vnames)]]
  dt[,nepsy_understanding_score := nepsy_understanding_sum1 + nepsy_understanding_sum2,by = c("PREG_ID_299","BARN_NR")]
  
  # Visuospatial Processing Domain
  setnames(dt,"NY1_1","NEPSY.VISPROC.DesignCopying")
  # Attention and Executive Functioning
  setnames(dt,"NY6_1","NEPSY.INHIB.Statue")
  # Language
  setnames(dt,"nepsy_understanding_score","NEPSY.LANG.ComprehInstr")
  setnames(dt,"NY4_2","NEPSY.LANG.PhonProc")
  # Visual Attention
  setnames(dt,"NY5_2_3","NEPSY.VISATT.cats_score")
  setnames(dt,"NY5_2_4","NEPSY.VISATT.cats_time")
  setnames(dt,"NY5_3_3","NEPSY.VISATT.bunniescats_score")
  setnames(dt,"NY5_3_4","NEPSY.VISATT.bunniescats_time")
  
  #####################################################
  ############## Boston naming task ##################
  #####################################################
  bnt = data.table(read_sav("savs/BNT.sav"))
  bnt$BNT.sumscore = rowSums(bnt[,names(bnt)[grep("BN1_",names(bnt))],with = F] < 5)
  dt = merge(dt,bnt,by = c("PREG_ID_299","BARN_NR"))
  rm(bnt)
  #####################################################
  ################ COOKIE DELAY TASK ##################
  #####################################################
  cdt = data.table(read_sav("savs/CDT.sav"))
  setnames(cdt,"CD1_2","CDT.sumscore")
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
  setnames(stp,"SNURR1_5","STP.score")
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
  setnames(gpt,"GPT.dominanthand","GP1")
  setnames(gpt,"GP2_1","GPT.dominant.seconds")
  setnames(gpt,"GP2_2","GPT.dominant.numbermissed")
  setnames(gpt,"GP2_3","GPT.dominant.number2hands")
  setnames(gpt,"GP2_1","GPT.ndomnt.seconds")
  setnames(gpt,"GPT.ndomnt.numbermissed","GP2_2")
  setnames(gpt,"GPT.ndomnt.number2hands","GP2_3")
  
  dt = merge(dt,gpt,by = c("PREG_ID_299","BARN_NR"))
  rm(gpt)
  return(dt)
}