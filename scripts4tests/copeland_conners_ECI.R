get_Copland = function(qu_a,qu_b){
  # COPLAND 
  # http://www.rubin-lab.umd.edu/pubs/Downloadable%20pdfs/kenneth_rubin/Inhibition,%20shyness,%20withdrawal/social%20withdrawal/nonsocial%20play%20Coplan%20Rubin%20Soc%20Dev%201998.pdf
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
  
  PPS_vars = names(qu_a)[1:2]
  for (d in names(items2dimensions)){
    old_names_a = paste0("BHCOPLAND",items2dimensions[[d]])
    old_names_b = paste0("B__1",sub(" ","_",sprintf("%2i",items2dimensions[[d]])))
    new_names = paste0(paste0("PPS.T.",d,".I."),items2dimensions[[d]])
    setnames(qu_a,old_names_a,new_names)
    setnames(qu_b,old_names_b,new_names)
    PPS_vars = c(PPS_vars,new_names)
  }
  for (v in PPS_vars[-c(1,2)])  {
    qu_b[[v]] = qu_b[[v]]-1
    attributes(qu_b[[v]]) = attributes(qu_a[[v]])
    }
  PPS = rbind(qu_a[,PPS_vars,with = F],qu_b[,PPS_vars,with = F])
  for (v in names(items2dimensions)) {
    PPS = make_sum_scores(PPS,PPS_vars[grep(v,PPS_vars)],paste0("PPS.T.SS.",v))
  }
  return(PPS)
  
  abbreviations = c(PPS = "Preschool Play Behavior Scale")
}

get_Conners = function(qu_a,qu_b){
  # Conners
  # A BHCONNERS_B1 - BHCONNERS_B25
  
  items2dimensions = list(AIA = c(1,4,12,14,16,17,22,23), # 14, 16 and 23 right here?
                          AH = c(3,7,8,10,11,15,19,24,25),
                          ODD = c(2,6,9,18,5,20, 21)) # not sure about 20 and 21 here
  
  items2dimensions$ADHD = sort(c(items2dimensions[[1]],items2dimensions[[2]]))
  
  CRS_vars = names(qu_a)[1:2]
  for (d in names(items2dimensions)[1:3]){
    old_names_a = paste("BHCONNERS_B",items2dimensions[[d]],sep = "")
    new_names = paste(paste("CS.T.",d,".I.",sep = ""),items2dimensions[[d]],sep = "")
    setnames(qu_a,old_names_a,new_names)
    CRS_vars = c(CRS_vars,new_names)
  }
  
  for (v in CRS_vars[-c(1,2)]) {
    qu_b[[v]] = NA
    attributes(qu_b[[v]]) = attributes(qu_a[[v]])
    }
  
  CRS = rbind(qu_a[,CRS_vars,with = F],qu_b[,CRS_vars,with = F])
  
  for (v in names(items2dimensions)) {
    CRS = make_sum_scores(CRS,names(CRS)[grep(v,names(CRS))], paste0("CS.T.",v,".SS"))
  }
  abbreviations = c(AIA = "ADHD, inattention",
                    AH = "ADHD, hyperactivity",
                    ADHD = "ADHD",
                    ODD = "ODD",
                    SS = "sum of scores")
  return(CRS)
}




get_ECI = function(qu_a,qu_b){
  # ECI
  # The Standardization of Early Childhood Inventory-4 (Eci-4) on Romanian Population-a Preliminary ReportIncomplete
  # BALAJ A, Albu M, Porumb M, Miclea M Cognition, Brain, Behavior. An Interdisciplinary Journal
  # http://www.harkinsandassociatesinc.com/harkins/docs/files/NP%20Packet%20for%203-4%20year%20olds.pdf
  
  items2dimensions = list(ADHD = 1:19,AIA = 1:9,AH = 10:18,
                          ODD = 19:26,CD = 27:35,
                          PCS = 36:45, SpecificPhobia  = 46,
                          Obsessions = 47, Compulsions = 48,
                          Motortics = 49, Vocaltics = 50,
                          GenAnx = 51:53, SelMut = 54, 
                          SepAnx = 55, MajDep = 58:66,
                          AdjDiso = 67, SocPhob = 68:70,
                          ElimPrbl = 71:72, PTSD = 73:75, 
                          AutSpec = 76:87)
  ECI_vars = names(qu_a)[1:2]
  for (d in names(items2dimensions)[-1]){
    old_names_b = paste0("B__4",sub(" ","_",sprintf("%2i",items2dimensions[[d]])))
    new_names = paste0(paste0("ECI.T.",d,".I"),items2dimensions[[d]])
    setnames(qu_b,old_names_b,new_names)
    ECI_vars = c(ECI_vars,new_names)
  }
  for (v in ECI_vars[-c(1,2)]) qu_a[[v]] = NA
  
  ECI = rbind(qu_a[,ECI_vars,with = F],qu_b[,ECI_vars,with = F])
  
  for (v in names(items2dimensions)) {
    ECI = make_sum_scores(ECI,names(ECI)[grep(v,names(ECI))], paste0("ECI.T.",v,".SS"))
  }
  return(ECI)
}