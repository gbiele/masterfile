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

cdi = get_cdi(pqa,pqb)
sdq = get_sdq(pqa,pqb)
cbq = get_cbq_eas(pqa,pqb)
# brief is missing!

#####################################################
################ Parent questionnaires ##############
#####################################################
source("cdi.R")
source("sdq.R")

pqa = data.table(read_sav("savs/SBF.sav"))
pqb = data.table(read_sav("savs/ADHD13_SBF.sav"))

cdi = get_cdi(pqa,pqb)
sdq = get_sdq(pqa,pqb)
cbq = get_cbq_eas(pqa,pqb)
