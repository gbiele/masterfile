file.sources = paste("scripts4tests/",list.files(path = "scripts4tests/",pattern="*.R"),sep = "")
tmp = sapply(file.sources,source,.GlobalEnv)
rm(tmp,file.sources)

data_dir = "../ADHD_3aar/PDB1717_"
index_vars = c( "PREG_ID_1717", "BARN_NR")

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
age = data.table(read_sav("../PDB1717_AlderKlinikk.sav"))[,c(index_vars,"Kontroll_Alder"),with = F]
age = merge(age,data.table(read_sav("../PDB1717_MFR_410_v8.sav"))[,c(index_vars,"KJONN"),with = F],by = index_vars,all.y = F)
setnames(age,"KJONN","Gender")
age$Gender = factor(age$Gender,labels  = c("boy","girl"))

MASTER = merge(MASTER,get_StanfordBinet(age),by = index_vars,all = T)

#####################################################
################ Parent questionnaires ##############
#####################################################

pqa = data.table(read_sav(paste0(data_dir,"SBF.sav")))
pqb = data.table(read_sav(paste0(data_dir,"ADHD13_SBF.sav")))

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

kgqa = data.table(read_sav(paste0(data_dir,"BHG.sav")))
kgqb = data.table(read_sav(paste0(data_dir,"ADHD6_BHG.sav")))

MASTER = merge(MASTER,get_cdi_kg(kgqa,kgqb),by = index_vars,all = T)
MASTER = merge(MASTER,get_sdq(kgqa,kgqb,"T"),by = index_vars,all = T)
MASTER = merge(MASTER,get_brief(kgqa,kgqb,"T"),by = index_vars,all = T)
MASTER = merge(MASTER,get_conners(kgqa,"T"),by = index_vars,all = T)
MASTER = merge(MASTER,get_Copland(kgqa,kgqb),by = index_vars,all = T)
MASTER = merge(MASTER,get_eci(kgqb,"T"),by = index_vars,all = T)
MASTER$VERSION = factor(1+MASTER[,get(index_vars[1])] %in% kgqb[,get(index_vars[1])])
rm(kgqa,kgqb)
# no cbq in kindergarden cbq = get_cbq_eas(pqa,pqb)

MASTER = MASTER[,-which(colSums(is.na(MASTER)) == nrow(MASTER)),with = F]

################# corrections #######################
# GENERELT: slette barn med PREG_ID_299 = 50163 fra alle tester, grunnet usikkerhet rundt barnets norskkunnskaper (dette er inkludert i alle endelige syntakser). 
# Mor oversetter nær sagt alle testinstruksjoner til serbisk, vi har ikke kontroll på hva hun sier.
is50163 = which(MASTER$Age_in_days == 1215 & MASTER$PP.ADHD.SS == 14)
MASTER = MASTER[-is50163,]

# Vær obs på sakene 50163 og 87831 når disse syntaksene kjøres. Ingen av disse sakene skal ha valid ABIQ!!
is87831 = which(MASTER$Age_in_days == 1294 & MASTER$PP.ADHD.SS == 3 & MASTER$PP.ODD.SS == 4)

MASTER[["StB.ABIQ.S"]][is87831] = NA
MASTER[["StB.ABIQ.PR"]][is87831] = NA
MASTER[["StB.WMindex.S"]][is87831] = NA

# Nina: slette BNT skåre til barnet der mor oversetter alle testinstruksjonene.
MASTER[["BNT.S"]][is87831] = NA

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

MASTER_scores = MASTER[,scores,with = F]

par(ps = 12)
pdf(file = "histograms.pdf",width = 29/2.54,height = 21/2.54,pointsize = 10)
hist_by_version(MASTER_scores[,-c(1,2,4,grep("\\.GR$",names(MASTER_scores))),with = F])
dev.off()


hists(MASTER_scores[,grep("\\.SC$",names(MASTER_scores)),with = F])

impdata = MASTER_scores[,-grep("\\.GR$",names(MASTER_scores)),with = F]

#write.foreign(MASTER[,1:2,with = F], paste0(getwd(),"/MASTER.txt"), paste0(getwd(),"/MASTER.sps"),   package="SPSS")
save(MASTER,file = "masterfile.Rdata")
save(MASTER_scores,file = "masterfile_scores.Rdata")

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

write.table(variable_info,file = "varivariable_info.dat",sep = "\t")

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