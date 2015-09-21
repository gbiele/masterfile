# things to add:
# - konklusion
# - depression
# - eating

# sanity checks: 
# - effecst of gender
# - effects of age

file.sources = paste("scripts4tests/",list.files(path = "scripts4tests/",pattern="*.R"),sep = "")
tmp = sapply(file.sources,source,.GlobalEnv)
rm(tmp,file.sources)

index_vars = c("PREG_ID_299","BARN_NR")

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
MASTER = merge(MASTER,get_PAPA(),by = index_vars,all = T)



#####################################################
################## Stanford Binet ###################
#####################################################
MASTER = merge(MASTER,get_ADHD_SCALE_Q6(),by = index_vars,all = T)

#####################################################
# screeing scale from Q6, filled out in ADHD Study ##
#####################################################
MASTER = merge(MASTER,get_StanfordBinet(),by = index_vars,all = T)

#####################################################
################ Parent questionnaires ##############
#####################################################

pqa = data.table(read_sav("savs/SBF.sav"))
pqb = data.table(read_sav("savs/ADHD13_SBF.sav"))

MASTER = merge(MASTER,get_cdi(pqa,pqb,"P"),by = index_vars,all = T)
MASTER = merge(MASTER,get_sdq(pqa,pqb,"P"),by = index_vars,all = T)
MASTER = merge(MASTER,get_brief(pqa,pqb,"P"),by = index_vars,all = T)
MASTER = merge(MASTER,get_cbq_eas(pqa,pqb) ,by = index_vars,all = T)
MASTER = merge(MASTER,get_eci(pqb,"P"),by = index_vars,all = T)
MASTER = merge(MASTER,get_conners(pqa,"P"),by = index_vars,all = T)
MASTER = merge(MASTER,get_diagnoses(pqa,pqb),by = index_vars,all = T)
MASTER = merge(MASTER,get_family_illness(pqa,pqb),by = index_vars,all = T)



rm(pqa,pqb)
#####################################################
################ teacher questionnaires ##############
#####################################################

kgqa = data.table(read_sav("savs/BHG.sav"))
kgqb = data.table(read_sav("savs/ADHD6_BHG.sav"))

MASTER = merge(MASTER,get_cdi_kg(kgqa,kgqb),by = index_vars,all = T)
MASTER = merge(MASTER,get_sdq(kgqa,kgqb,"T"),by = index_vars,all = T)
MASTER = merge(MASTER,get_brief(kgqa,kgqb,"T"),by = index_vars,all = T)
MASTER = merge(MASTER,get_conners(kgqa,"T"),by = index_vars,all = T)
MASTER = merge(MASTER,get_Copland(kgqa,kgqb),by = index_vars,all = T)
MASTER = merge(MASTER,get_eci(kgqb,"T"),by = index_vars,all = T)
MASTER$VERSION = factor(MASTER$PREG_ID_299 %in% kgqb$PREG_ID_299+1)
rm(kgqa,kgqb)
# no cbq in kindergarden cbq = get_cbq_eas(pqa,pqb)

################# corrections #######################
# GENERELT: slette barn med PREG_ID_299 = 50163 fra alle tester, grunnet usikkerhet rundt barnets norskkunnskaper (dette er inkludert i alle endelige syntakser). 
# Mor oversetter nær sagt alle testinstruksjoner til serbisk, vi har ikke kontroll på hva hun sier.
is50163 = which(MASTER$Age_in_days == 1215 & MASTER$PP.ADHD.SS == 14)
MASTER = MASTER[-is50163,]

# Vær obs på sakene 50163 og 87831 når disse syntaksene kjøres. Ingen av disse sakene skal ha valid ABIQ!!
is87831 = which(MASTER$Age_in_days == 1294 & MASTER$PP.ADHD.SS == 3 & MASTER$PP.ODD.SS == 4)

MASTER[["SB.ABIQ.S"]][is87831] = NA
MASTER[["SB.ABIQ.PR"]][is87831] = NA
MASTER[["SB.WMindex.S"]][is87831] = NA

# Nina: slette BNT skåre til barnet der mor oversetter alle testinstruksjonene.
MASTER[["BNT.S"]][is87831] = NA

# bnt$BNT.SCORE = rowSums(bnt[,names(bnt)[grep("BN1_",names(bnt))],with = F] < 5)
# bnt[PREG_ID_299 == 50163, BNT.SCORE := NA] 

rm(list = (setdiff(ls()[!(ls() %in% lsf.str())],c("MASTER","index_vars"))))

## remove cases with missing ADHD diagnosis
MASTER = MASTER[!is.na(PP.ADHD.GR),]

MASTER[,Age_quartiles := cut(Age_in_days,quantile(Age_in_days,na.rm = T,(0:4/4)),labels = 1:4)]
MASTER[,Age_tercile := cut(Age_in_days,quantile(Age_in_days,na.rm = T,(0:3/3)),labels = 1:3)]


MASTER = MASTER[,c(index_vars,sort(names(MASTER)[-c(1:2)])),with = F]

MASTER[,fAge_tercile := factor(Age_tercile)]
MASTER[,fGender := factor(Gender)]

scores = c(index_vars,
           "VERSION",
           names(MASTER)[grep("\\.S$|\\.SS$|\\.GR|Age|Gender",names(MASTER))])

MASTER_scores = MASTER[,scores,with = F]


hist_by_version(MASTER_scores[,-grep("\\.GR$",names(MASTER_scores)),with = F])

impdata = MASTER_scores[,-grep("\\.GR$",names(MASTER_scores)),with = F]

#write.foreign(MASTER[,1:2,with = F], paste0(getwd(),"/MASTER.txt"), paste0(getwd(),"/MASTER.sps"),   package="SPSS")
save(MASTER,file = "masterfile.Rdata")
save(MASTER_scores,file = "masterfile_scores.Rdata")

#plot_my_hists(MASTER_scores)
#make_correlation_plot(MASTER_scores)

## note: select a subset of variables, if you want to look only at those
# e.g. 
# attention_scores = names(MASTER_scores)[grep("att|Att|ATT",names(MASTER_scores))]
# make_correlation_plot(MASTER_scores[,attention_scores,with = F],save_image = F)
# for a large number of variables use save_image = T (this is also the default) to
# save the image as heatmap.png, which can be inspeacted more easily