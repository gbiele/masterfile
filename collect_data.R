library(haven)
library(data.table)
library(car)

file.sources = paste("scripts4tests/",list.files(path = "scripts4tests/",pattern="*.R"),sep = "")
tmp = sapply(file.sources,source,.GlobalEnv)
rm(tmp,file.sources)

#####################################################
############# Neuropsychological tests ##############
#####################################################
dt = get_neuropsych()

#####################################################
################ Parent questionnaires ##############
#####################################################

pqa = data.table(read_sav("savs/SBF.sav"))
pqb = data.table(read_sav("savs/ADHD13_SBF.sav"))

cdi = get_cdi(pqa,pqb,"parent")
sdq = get_sdq(pqa,pqb,"parent")
cbq = get_cbq_eas(pqa,pqb) 
brief = get_brief(pqa,pqb,"parent")
child_diags = get_diagnoses(pqa,pqb)
fam_health = get_family_illness(pqa,pqb)


#####################################################
################ Parent questionnaires ##############
#####################################################

qu_a = data.table(read_sav("savs/BHG.sav"))
qu_b = data.table(read_sav("savs/ADHD6_BHG.sav"))

cdi_kg = get_cdi(kgqa,kgqb,"teacher")
sdq_kg = get_sdq(qu_a,qu_b,"teacher")
# no cbq in kindergarden cbq = get_cbq_eas(pqa,pqb)
brief_kg = get_brief(qu_a,qu_b,"teacher")
child_diags = get_diagnoses(pqa,pqb)
fam_health = get_family_illness(pqa,pqb)
