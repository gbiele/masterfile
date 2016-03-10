

get_eci = function(qu,rater){
  
  
  if (rater == "P") {
    eci = qu[,c(index_vars,names(qu)[grep("C_30",names(qu))]),with = F]
    items2dims = list(AIA = 1:9,
                      AHY = 10:15,
                      AIM = 16:18,
                      ODD = 21:28,
                      CD = 29:38,
                      PCS = 39:48,
                      SEA = 49:56,
                      PHOB = 57, OBS = 58, CMP = 59, MOT = 60, VOT = 61,
                      GEA = 62:64,
                      SMU = 65,
                      DEP = 66:77,
                      SPH = 78:80,
                      SLP = 81:86,
                      ELM = 87:88,
                      PTSD = 89:92,
                      FEED = 93:94,
                      RAD = 95:96,
                      AUT = 97:108)
    base = "C_30"
    nbase = "ECI.P."
  } else if (rater == "T") {
    eci = qu[,c(index_vars,names(qu)[grep("B__4",names(qu))]),with = F]
    items2dims = list(AIA = 1:9,
                      AHY = 10:15,
                      AIM = 16:18,
                      ODD = 19:26,
                      CD = 27:35,
                      PCS = 36:45,
                      PHOB = 46, OBS = 47, CMP = 48, MOT = 49, VOT = 50,
                      GEA = 51:53,
                      SMU = 54,
                      DEP = 58:67,
                      SPH = 68:70,
                      ELM = 71:72,
                      PTSD = 73:75,
                      AUT = 76:87)
    base = "B__4"
    nbase = "ECI.T."
  }
  
  for (v in names(items2dims)) {
    if(rater == "P") {
      old_names = paste0(base,gsub("_0","__",gsub("^0","_",sprintf("%03d", items2dims[[v]]))))
    } else {
      old_names = paste0(base,gsub("^0","_",sprintf("%02d",items2dims[[v]])))
    }
    
    new_names = paste0(nbase,v,".i",items2dims[[v]])
    setnames(eci,old_names,new_names)
    eci = make_sum_scores(eci,new_names,paste0(nbase,v,".SS"),count_cutoff_idx = 2)
  }
  eci = make_sum_scores(eci,
                        unlist(lapply(names(items2dims[2:3]), 
                                      function(v) paste0(nbase,v,".i",items2dims[[v]]))),
                        paste0(nbase,"AHI.SS"),
                        count_cutoff_idx = 2)
  eci = make_sum_scores(eci,
                        unlist(lapply(names(items2dims[1:3]), 
                                      function(v) paste0(nbase,v,".i",items2dims[[v]]))),
                        paste0(nbase,"ADHD.SS"),
                        count_score = T,
                        count_cutoff_idx = 2)
  
  abbreviations = c(ADHD = "ADHD",
                    ECI = "ECI4",
                    T = "Teacher ratings",
                    P = "Parent ratings",
                    SS = "sum of scores",
                    SC = "count of non-zero scores",
                    AIA = "ADHD inattention",
                    AHY = "ADHD hyperactivity",
                    AIM = "ADHD impulsivity",
                    AHI = "ADHD Hyperactivity & Impulsivity",
                    CD = "Conduct disorder",
                    ODD = "Oppositional defiant disorder",
                    SPH = "Social phobia",
                    PCS = "Peer conflict",
                    SEA = "Separation Anxiety",
                    PHOB = "Specific phobia",
                    OBS = "Obsessions",
                    CMP = "Compulsions",
                    MOT = "Motor tics",
                    VOT = "Vocal tics",
                    GEA = "Generalized anxiety",
                    SMU = "Selective mutism",
                    DEP = "Major depressive disorder",
                    MAD = "Major audjustment disorder",
                    SLP = "Sleep disorders",
                    ELM = "Elimination disorder",
                    PTSD = "Post traumatic stress disorder",
                    FEED = "Feeding disorder",
                    RAD = "Reactive attachment disorder",
                    AUT = "Autism spectrum disorder")
  
  eci = eci[,c(index_vars,names(eci)[grep("^ECI",names(eci))]),with = F]
  
  eci = add_label(eci,"ECI",abbreviations)
  eci_labels = c(Never = 1, Sometimes = 2, Often = 3, VeryOften = 4)
  for (v in names(eci)[grep("i[0-9]",names(eci))]) {
    eci[[v]] = labelled(eci[[v]],labels = eci_labels)
  }
  
  return(eci)
}

