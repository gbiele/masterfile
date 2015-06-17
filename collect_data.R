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
dt = get_neuropsych()

#####################################################
################## PAPA interview ###################
#####################################################
PAPA = get_PAPA()

#####################################################
################## Stanford Binet ###################
#####################################################
PAPA = get_StanfordBinet()


#####################################################
################ Parent questionnaires ##############
#####################################################

pqa = data.table(read_sav("savs/SBF.sav"))
pqb = data.table(read_sav("savs/ADHD13_SBF.sav"))

cdi = get_cdi(pqa,pqb,"parent")
sdq = get_sdq(pqa,pqb,"parent")
brief = get_brief(pqa,pqb,"parent")
cbq = get_cbq_eas(pqa,pqb) 
child_diags = get_diagnoses(pqa,pqb)
fam_health = get_family_illness(pqa,pqb)

rm(pqa,pqb)
#####################################################
################ teacher questionnaires ##############
#####################################################

kgqa = data.table(read_sav("savs/BHG.sav"))
kgqb = data.table(read_sav("savs/ADHD6_BHG.sav"))

cdi_kg = get_cdi_kg(kgqa,kgqb)
sdq_kg = get_sdq(kgqa,kgqb,"teacher")
brief_kg = get_brief(kgqa,kgqb,"teacher")
rm(kgqa,kgqb)
# no cbq in kindergarden cbq = get_cbq_eas(pqa,pqb)