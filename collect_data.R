library(haven)
library(data.table)
library(car)

file.sources = paste("scripts4tests/",list.files(path = "scripts4tests/",pattern="*.R"),sep = "")
tmp = sapply(file.sources,source,.GlobalEnv)
rm(tmp,file.sources)

#####################################################
############# Neuropsychological tests ##############
#####################################################
# insruments:
# - Nepsy (visuo spatial, visual attention, understanding, executive functioning)
# - Boston nameing Task
# - Cookie delay Task
# - Truck reversla Task
# - Spin the pots
# - grooved pegboard
# - 
MASTER = get_neuropsych()

#####################################################
################## PAPA interview ###################
#####################################################
MASTER = merge(MASTER,get_PAPA(),by = c("PREG_ID_299","BARN_NR"),all = T)

#####################################################
################## Stanford Binet ###################
#####################################################
MASTER = merge(MASTER,get_ADHD_SCALE_Q6(),by = c("PREG_ID_299","BARN_NR"),all = T)

#####################################################
# screeing scale from Q6, filled out in ADHD Study ##
#####################################################
MASTER = merge(MASTER,get_StanfordBinet(),by = c("PREG_ID_299","BARN_NR"),all = T)


#####################################################
################ Parent questionnaires ##############
#####################################################

pqa = data.table(read_sav("savs/SBF.sav"))
pqb = data.table(read_sav("savs/ADHD13_SBF.sav"))

MASTER = merge(MASTER,get_cdi(pqa,pqb,"parent"),by = c("PREG_ID_299","BARN_NR"),all = T)
MASTER = merge(MASTER,get_sdq(pqa,pqb,"parent"),by = c("PREG_ID_299","BARN_NR"),all = T)
MASTER = merge(MASTER,get_brief(pqa,pqb,"parent"),by = c("PREG_ID_299","BARN_NR"),all = T)
MASTER = merge(MASTER,get_cbq_eas(pqa,pqb) ,by = c("PREG_ID_299","BARN_NR"),all = T)
MASTER = merge(MASTER,get_diagnoses(pqa,pqb),by = c("PREG_ID_299","BARN_NR"),all = T)
MASTER = merge(MASTER,get_family_illness(pqa,pqb),by = c("PREG_ID_299","BARN_NR"),all = T)

rm(pqa,pqb)
#####################################################
################ teacher questionnaires ##############
#####################################################

kgqa = data.table(read_sav("savs/BHG.sav"))
kgqb = data.table(read_sav("savs/ADHD6_BHG.sav"))

MASTER = merge(MASTER,get_cdi_kg(kgqa,kgqb),by = c("PREG_ID_299","BARN_NR"),all = T)
MASTER = merge(MASTER,get_sdq(kgqa,kgqb,"teacher"),by = c("PREG_ID_299","BARN_NR"),all = T)
MASTER = merge(MASTER,get_brief(kgqa,kgqb,"teacher"),by = c("PREG_ID_299","BARN_NR"),all = T)
MASTER = merge(MASTER,get_Conners(kgqa,kgqb),by = c("PREG_ID_299","BARN_NR"),all = T)
MASTER = merge(MASTER,get_Copland(kgqa,kgqb),by = c("PREG_ID_299","BARN_NR"),all = T)
rm(kgqa,kgqb)
# no cbq in kindergarden cbq = get_cbq_eas(pqa,pqb)



################# corrections #######################
# GENERELT: slette barn med PREG_ID_299 = 50163 fra alle tester, grunnet usikkerhet rundt barnets norskkunnskaper (dette er inkludert i alle endelige syntakser). 
# Mor oversetter nær sagt alle testinstruksjoner til serbisk, vi har ikke kontroll på hva hun sier.
is50163 = which(MASTER$Age_in_days == 1215 & MASTER$PAPA.ADHD.sum.SCORE == 14)
MASTER = MASTER[-is50163,]

# Vær obs på sakene 50163 og 87831 når disse syntaksene kjøres. Ingen av disse sakene skal ha valid ABIQ!!
is87831 = which(MASTER$Age_in_days == 1294 & MASTER$PAPA.ADHD.sum.SCORE == 3 & MASTER$PAPA.BH.ODD.sum.SCORE == 4)

MASTER[is87831, StBn.SCORE.ABIQ := NA]
MASTER[is87831, StBn.SCORE.PercRank := NA]
MASTER[is87831, StBn.SCORE.WMindex := NA]

# Nina: slette BNT skåre til barnet der mor oversetter alle testinstruksjonene.
MASTER[is87831,BNT.SCORE := NA]

# bnt$BNT.SCORE = rowSums(bnt[,names(bnt)[grep("BN1_",names(bnt))],with = F] < 5)
# bnt[PREG_ID_299 == 50163, BNT.SCORE := NA] 

rm(is87831,is50163)

save(MASTER,file = "masterfile.Rdata")

MASTER_scores = MASTER[,grep("SCORE|COUNT",names(MASTER)),with = F]
missing_more_than_40_sumscores =  which((apply(is.na(MASTER_scores),1,sum)>40))
MASTER_scores = MASTER_scores[-missing_more_than_40_sumscores,]

save(MASTER_scores,file = "masterfile_scores.Rdata")


#plot_my_hists(MASTER_scores)