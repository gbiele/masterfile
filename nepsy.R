library(haven)
library(data.table)

# all tests in here?
# https://www.duo.uio.no/bitstream/handle/10852/17905/X.pdf?sequence=1

#####################################################
######################## nepsy ######################
#####################################################
# http://books.google.no/books/about/Essentials_of_NEPSY_Assessment.html?id=PRxWMaVeVX4C&redir_esc=y

################ QUESTIONS ##############
# what is with NA values?

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
dt[,score.NEPSY.VISPROC.DesignCopying := NY1_1]
# Attention and Executive Functioning
dt[,score.NEPSY.INHIB.Statue := NY6_1]
# Language
dt[,score.NEPSY.LANG.ComprehInstr := nepsy_understanding_score]
dt[,score.NEPSY.LANG.PhonProc := NY4_2]
# Visual Attention
dt[,score.NEPSY.VISATT.cats_score := NY5_2_3]
dt[,score.NEPSY.VISATT.cats_time := NY5_2_4]
dt[,score.NEPSY.VISATT.bunniescats_score := NY5_3_3]
dt[,score.NEPSY.VISATT.bunniescats_time := NY5_3_4]

#####################################################
############## Boston naming task ##################
#####################################################
bnt = data.table(read_sav("savs/BNT.sav"))
bnt$score.BNT.sumscore = rowSums(bnt[,names(bnt)[grep("BN1_",names(bnt))],with = F] < 5)
dt = merge(dt,bnt,by = c("PREG_ID_299","BARN_NR"))
rm(bnt)
#####################################################
################ COOKIE DELAY TASK ##################
#####################################################
cdt = data.table(read_sav("savs/CDT.sav"))
cdt[,socre.CDT.sumscore := CD1_2]
dt = merge(dt,cdt,by = c("PREG_ID_299","BARN_NR"))
rm(cdt)
#####################################################
################ COOKIE DELAY TASK ##################
#####################################################
cdt = data.table(read_sav("savs/CDT.sav"))
cdt[,socre.CDT.sumscore := CD1_2]
dt = merge(dt,cdt,by = c("PREG_ID_299","BARN_NR"))
rm(cdt)

#####################################################
############# TRUCK REVERSAL LEARNING ###############
#####################################################
trl = data.table(read_sav("savs/TRLT.sav"))
trl[,score.TRLT.A.learned := TR1_2_1]
trl[,score.TRLT.A.numbererrors := TR1_3]
trl[,score.TRLT.A.trials2crit := TR1_4]
trl[,score.TRLT.B.learned := TR2_2_1]
trl[,score.TRLT.B.numbererrors := TR2_3]
trl[,score.TRLT.B.trials2crit := TR2_4]
dt = merge(dt,trl,by = c("PREG_ID_299","BARN_NR"))
rm(trl)

#####################################################
############# TRUCK REVERSAL LEARNING ###############
#####################################################
# https://paperpile.com/view/49565dba-6695-029a-ac69-7e1d9bd95028
# Hughes & ENsor 2005
trl = data.table(read_sav("savs/TRLT.sav"))
trl[,score.TRLT.A.learned := TR1_2_1]
trl[,score.TRLT.A.numbererrors := TR1_3]
trl[,score.TRLT.A.trials2crit := TR1_4]
trl[,score.TRLT.B.learned := TR2_2_1]
trl[,score.TRLT.B.numbererrors := TR2_3]
trl[,score.TRLT.B.trials2crit := TR2_4]
dt = merge(dt,trl,by = c("PREG_ID_299","BARN_NR"))
rm(trl)

#####################################################
################## Spin the Pots ####################
#####################################################
# https://paperpile.com/view/49565dba-6695-029a-ac69-7e1d9bd95028
# Hughes & ENsor 2005

stp = data.table(read_sav("savs/SnurrB.sav"))
stp[,score.STP.trials2crit := SNURR1_1]
stp[,score.STP.numerrorsempty := SNURR1_2]
stp[,score.STP.numerrorsfull := SB1_3]
stp[,score.STP.totalerrors := SNURR1_4]
stp[,score.STP.score := SNURR1_5]
stp[,score.STP.impulsopenings := SNURR1_8]
dt = merge(dt,stp,by = c("PREG_ID_299","BARN_NR"))
rm(stp)

