file.sources = paste("scripts4tests/",list.files(path = "scripts4tests/",pattern="*.R"),sep = "")
tmp = sapply(file.sources,source,.GlobalEnv)
rm(tmp,file.sources)

data_dir = "savs/"
index_vars = c( "PREG_ID_299", "BARN_NR")

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
MASTER = get_neuropsych(data_dir)

#####################################################
########### PAPA interview & conclusion #############
#####################################################
MASTER = merge(MASTER,get_PAPA299(),by = index_vars,all = T)
MASTER = merge(MASTER,get_conclusion(),by = c(index_vars),all = T)


#####################################################
# screeing scale from Q6, filled out in ADHD Study ##
#####################################################
MASTER = merge(MASTER,get_ADHD_SCALE_Q6(),by = index_vars,all = T)

#####################################################
################## Stanford Binet ###################
#####################################################


#age = NaN2NA(data.table(read_sav("../PDB1717_AlderKlinikk.sav")))[,c(index_vars,"Kontroll_Alder"),with = F]
#age = NaN2NA(merge(age,data.table(read_sav("../PDB1717_MFR_410_v8.sav")))[,c(index_vars,"KJONN"),with = F],by = index_vars,all.y = F)
#setnames(age,"KJONN","Gender")
#age$Gender = factor(age$Gender,labels  = c("boy","girl"))

age = NaN2NA(data.table(read_sav("savs/ADHD_Score.sav")))
age$Gender = factor(mapvalues(age$Gender,c("X_MALE","X_FEMALE"),1:2),labels  = c("boy","girl"))
setnames(age,c("barn_nr","Sumscore"),c("BARN_NR","MoBa.Q6.ADHD.SS"))
age = age[,c("PREG_ID_299","BARN_NR","MoBa.Q6.ADHD.SS","Kontroll_Alder","ADHD_Source","Gender"),with = F]
age[,MoBa.Q6.ADHD.SS := MoBa.Q6.ADHD.SS-11]

MASTER = merge(MASTER,get_StanfordBinet(age),by = index_vars,all = T)

MASTER = merge(MASTER,age,by = c(index_vars),all.x = T, all.y = F)

setnames(MASTER,"Gender.y","Gender")
MASTER[,Gender.x := NULL]

#####################################################
################ Parent questionnaires ##############
#####################################################

pqa = NaN2NA(data.table(read_sav(paste0(data_dir,"SBF.sav"))))
pqb = NaN2NA(data.table(read_sav(paste0(data_dir,"ADHD13_SBF.sav"))))

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

kgqa = NaN2NA(data.table(read_sav(paste0(data_dir,"BHG.sav"))))
kgqb = NaN2NA(data.table(read_sav(paste0(data_dir,"ADHD6_BHG.sav"))))

MASTER = merge(MASTER,get_cdi_kg(kgqa,kgqb),by = index_vars,all = T)
MASTER = merge(MASTER,get_sdq(kgqa,kgqb,"T"),by = index_vars,all = T)
MASTER = merge(MASTER,get_brief(kgqa,kgqb,"T"),by = index_vars,all = T)
MASTER = merge(MASTER,get_conners(kgqa,"T"),by = index_vars,all = T)
MASTER = merge(MASTER,get_Copland(kgqa,kgqb),by = index_vars,all = T)
MASTER = merge(MASTER,get_eci(kgqb,"T"),by = index_vars,all = T)
MASTER$VERSION = factor(1+MASTER[,get(index_vars[1])] %in% kgqb[,get(index_vars[1])])
rm(kgqa,kgqb)
# no cbq in kindergarden cbq = get_cbq_eas(pqa,pqb)

################ Masternal age, edu, parity ##############
load("../MoBa/redSkjema1_PDB299_v6.Rdata")
SES = Q1r[,c("PREG_ID_299","AA1124","AA1125")]
SES[which(is.na(SES$AA1124) & SES$AA1125 > 1),"AA1124"] = SES[which(is.na(SES$AA1124) & SES$AA1125 > 1),"AA1125"]-1
SES[which(is.na(SES$AA1124) & SES$AA1125 == 1),"AA1124"] = 1
SES$mEDU.comp = cut(SES$AA1124,breaks = c(-.5,1.5,4.5,5.5,6.5), labels = c("Elementary", "High-School","Bachelor","Master"))
SES = SES[,c("PREG_ID_299","mEDU.comp")]

MFR = read_sav("../MoBa/MFR_350_PDB299_v6.sav")
MFR = MFR[,c("PREG_ID_299","BARN_NR","MORS_ALDER","PARITET_5","SVLEN","VEKT")]
names(MFR) = c("PREG_ID_299","BARN_NR","mAge","Parity","gest_age_wks","birthweight")


MASTER = merge(MASTER,MFR, by = c("PREG_ID_299","BARN_NR"))
MASTER = merge(MASTER,SES, by = c("PREG_ID_299"))
################# corrections #######################
# GENERELT: slette barn med PREG_ID_299 = 50163 fra alle tester, grunnet usikkerhet rundt barnets norskkunnskaper (dette er inkludert i alle endelige syntakser). 
# Mor oversetter nær sagt alle testinstruksjoner til serbisk, vi har ikke kontroll på hva hun sier.
#is50163 = which(MASTER$Age_in_days == 1215 & MASTER$PP.ADHD.SS == 14)
#MASTER = MASTER[-is50163,]

# Vær obs på sakene 50163 og 87831 når disse syntaksene kjøres. Ingen av disse sakene skal ha valid ABIQ!!
#is87831 = which(MASTER$Age_in_days == 1294 & MASTER$PP.ADHD.SS == 3 & MASTER$PP.ODD.SS == 4)
#MASTER[["StB.ABIQ.S"]][is87831] = NA
#MASTER[["StB.ABIQ.PR"]][is87831] = NA
#MASTER[["StB.WMindex.S"]][is87831] = NA

# Nina: slette BNT skåre til barnet der mor oversetter alle testinstruksjonene.
#MASTER[["BNT.S"]][is87831] = NA

# bnt$BNT.SCORE = rowSums(bnt[,names(bnt)[grep("BN1_",names(bnt))],with = F] < 5)

rm(list = (setdiff(ls()[!(ls() %in% lsf.str())],c("MASTER","index_vars"))))


MASTER = MASTER[,c(index_vars,sort(names(MASTER)[-c(1:2)])),with = F]

#MASTER[,fGender := factor(Gender)]


nms = data.frame( label = unlist(sapply(MASTER,function(x) attr(x,"label"))))
nms$varname = rownames(nms)
value_labels = data.frame(val_labels = unlist(sapply(MASTER,function(x) {ifelse(length(attributes(x)$labels) > 0,
                                                                                paste0(paste(attributes(x)$labels,names(attributes(x)$labels)),collapse = "; "),
                                                                                " ")})))
value_labels$varname = rownames(value_labels)
nms = merge(nms,value_labels,by = "varname")
write.csv(nms,"nms.txt")


scores = c(index_vars,
           "VERSION",
           names(MASTER)[grep("\\.S$|\\.SS$|\\.GR|\\.SC$|Age|Gender|.errors$|n_miss$|sec$|n2h$",names(MASTER))])
scores_plus = c(scores,"mEDU.comp","mAge","Parity","gest_age_wks","birthweight")
MASTER_scores = MASTER[,scores_plus,with = F]

########### 4 annette ############
# vars = c("StB.NVWMS.S", "StB.VWMS.S" ,"STP.S" ,
#          "TT.A.errors","TT.B.errors",
#          "NY.INHIB.Statue.S","CDT.S",
#          "MoBa.Q6.ADHD.SS","Kontroll_Alder",
#          "ADHD_Source")
# 
# D = MASTER[,c("StB.NVWMS.S", "StB.VWMS.S" ,"STP.S" ,
#               "TT.A.errors","TT.B.errors",
#               "NY.INHIB.Statue.S","CDT.S",
#               "MoBa.Q6.ADHD.SS","Kontroll_Alder",
#               "ADHD_Source","PP.ADHD.CAT","PP.ADHD.SS","PP.ADHD.SC"), with = F]
# D = D[!is.na(PP.ADHD.SS),]
# 
# save(D,file = "AnnettesData.Rdata")
# 
# #####################################

par(ps = 12)
pdf(file = "histograms_nni.pdf",width = 29/2.54,height = 21/2.54,pointsize = 10)
hist_by_version(MASTER_scores[,-c(1,2,4,grep("\\.GR$",names(MASTER_scores))),with = F])
dev.off()


hists(MASTER_scores[,grep("\\.SC$",names(MASTER_scores)),with = F])

impdata = MASTER_scores[,-grep("\\.GR$",names(MASTER_scores)),with = F]

#write.foreign(MASTER[,1:2,with = F], paste0(getwd(),"/MASTER.txt"), paste0(getwd(),"/MASTER.sps"),   package="SPSS")
save(MASTER,file = "masterfile_nni.Rdata")
save(MASTER_scores,file = "masterfile_scores_nni.Rdata")

variable_infoa = data.frame(unlist(lapply(MASTER_scores, function(x) attr(x,"label"))))
variable_infob = data.frame(unlist(lapply(MASTER_scores, function(x) paste(paste(names(attr(x,"labels")),
                                                                                 attr(x,"labels"),
                                                                                 sep = " = "),
                                                                           collapse = "; ") )))
names(variable_infoa) = "Variable_label"
names(variable_infob) = "Value_labels"
variable_infoa$Variable_name = row.names(variable_infoa)
variable_infob$Variable_name = row.names(variable_infob)
variable_info = merge(variable_infoa,
                      variable_infob,
                      by = "Variable_name")[,c("Variable_name","Variable_label","Value_labels")]
rm(variable_infoa,variable_infob)

write.table(variable_info,file = "variable_info_nni.dat",sep = "\t")

#writeSPSSfromLabelled(MASTER,paste0(getwd(),"/MASTER.csv"),"MASTER.sps")
#writeSPSSfromLabelled(MASTER_scores,paste0(getwd(),"/MASTER_scores.csv"),"MASTER_scores.sps")

#plot_my_hists(MASTER_scores)
#make_correlation_plot(MASTER_scores)

## note: select a subset of variables, if you want to look only at those
# e.g. 
# attention_scores = names(MASTER_scores)[grep("att|Att|ATT",names(MASTER_scores))]
# make_correlation_plot(MASTER_scores[,attention_scores,with = F],save_image = F)
# for a large number of variables use save_image = T (this is also the default) to
# save the image as heatmap.png, which can be inspeacted more easily