get_Copland = function(qu_a,qu_b){
  # COPLAND
  # A BHCOPLAND1 - BHCOPLAND21
  # B B__1_1 - B__1_21
  
  # 1. Talks to other children during play
  # 2. Plays by himself/herself, examining a toy or object
  # 3. Plays "rough-and-tumble" with other children
  # 4. Takes on the role of onlooker or spectator
  # 5. Plays "make-believe" with other children
  # 6. Engages in group play
  # 7. Engages in pretend play by himself/herself
  # 8. Plays alone, building things with blocks and/or other toys
  # 9. Wanders around aimlessly
  # 10. Plays in groups with (not just besides) other children
  # 11. Plays "make-believe", but not with other children
  # 12. Watches or listens to other children without trying to join in
  # 13. Plays "rough-and-tumble" with other children
  # 14. Plays by himself/herself, drawing, painting pictures, or doing puzzles
  # 15. Talks to other children during play
  # 16. Engages in pretend play with other children
  # 17. Plays alone, exploring toys and objects, trying to figure out how they work
  # 18. Remains alone and unoccupied, perhaps staring off into space
  # 19. Finds it important to keep his/her toys in order
  # 20. Wants to lead and decide what others can do during play
  # 21. Plays only a short while with each toy, does not stick to anything
  
  items2dimensions = list(Reticent = c(4,9,12,18),
                          SolitaryPassive = c(2,14,17),
                          SolitaryActive = c(7,8,11),
                          SocialPlay = c(1,5,6,10,15,16),
                          RoughPlay = c(3,13))
  
  PPBS_vars = names(qu_a)[1:2]
  for (d in names(items2dimensions)){
    old_names_a = paste("BHCOPLAND",items2dimensions[[d]],sep = "")
    old_names_b = paste("B__1",sub(" ","_",sprintf("%2i",items2dimensions[[d]])),sep = "")
    new_names = paste(paste("PPBS.teacher.",d,".item",sep = ""),items2dimensions[[d]],sep = "")
    setnames(qu_a,old_names_a,new_names)
    setnames(qu_b,old_names_b,new_names)
    PPBS_vars = c(PPBS_vars,new_names)
  }
  
  PPBS = rbind(qu_a[,PPBS_vars,with = F],qu_b[,PPBS_vars,with = F])
  for (v in names(items2dimensions)) {
    PPBS[[paste("PPBS.teacher.",v,".sum.SCORE",sep = "")]] = make_sum_scores(PPBS[,grep(v,names(PPBS)),with = F])
  }
  return(PPBS)
}

get_Conners = function(qu_a,qu_b){
  # Conners
  # A BHCONNERS_B1 - BHCONNERS_B25
  
  
  items2dimensions = list(ADHDA = c(1,4,12,14,16,17,22,23), # 14, 16 and 23 right here?
                          ADHDH = c(3,7,8,10,11,15,19,24,25),
                          ODD = c(2,6,9,18,5,20, 21)) # not sure about 20 and 21 here
  
  CRS_vars = names(qu_a)[1:2]
  for (d in names(items2dimensions)){
    old_names_a = paste("BHCONNERS_B",items2dimensions[[d]],sep = "")
    new_names = paste(paste("Conners.teacher.",d,".item",sep = ""),items2dimensions[[d]],sep = "")
    setnames(qu_a,old_names_a,new_names)
    CRS_vars = c(CRS_vars,new_names)
  }
  for (v in CRS_vars[-c(1,2)]) qu_b[[v]] = NA
  
  CRS = rbind(qu_a[,CRS_vars,with = F],qu_b[,CRS_vars,with = F])
  
  for (v in names(items2dimensions)) {
    CRS[[paste("Conners.teacher.",v,".sum.SCORE",sep = "")]] = make_sum_scores(CRS[,grep(v,names(CRS)),with = F])
  }
  
  return(CRS)
}




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