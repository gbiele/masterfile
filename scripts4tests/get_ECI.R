get_ECI = function(qu_a,qu_b){
  # ECI
  # The Standardization of Early Childhood Inventory-4 (Eci-4) on Romanian Population-a Preliminary ReportIncomplete
  # BALAJ A, Albu M, Porumb M, Miclea M Cognition, Brain, Behavior. An Interdisciplinary Journal
  
  items2dimensions = list(ADHD = 1:8,ODD = 19:26,CD = 27:35,
                        PCS = 36:45, SpecificPhobia  = 46,
                        Obsessions = 47, Compulsions = 48,
                        Motortics = 49, Vocaltics = 50,
                        GeneralizedAnxiety = 51:53, SelectiveMutism = 54, 
                        SeparationAnxiety = 55, MajorDepression = 58:66,
                        Adjustmentdisorder = 67, Socialphobia = 68:70,
                        EliminationProblems = 71:72, PTSD = 73:75, 
                        AutismSpectrum = 76:87)
  ECI_vars = names(qu_a)[1:2]
  for (d in names(items2dimensions)){
    old_names_b = paste("B__4",sub(" ","_",sprintf("%2i",items2dimensions[[d]])),sep = "")
    new_names = paste(paste("ECI.teacher.",d,".item",sep = ""),items2dimensions[[d]],sep = "")
    setnames(qu_b,old_names_b,new_names)
    ECI_vars = c(ECI_vars,new_names)
  }
  for (v in ECI_vars[-c(1,2)]) qu_a[[v]] = NA
  
  ECI = rbind(qu_a[,ECI_vars,with = F],qu_b[,ECI_vars,with = F])
}