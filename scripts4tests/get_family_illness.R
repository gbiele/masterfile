get_family_illness = function(pqa,pqb){
  diagnosis = list(FH.skin = c("C_25_1","NO"),
                   FH.asthma_lung = c("C_25_2","SBF16A"), # and SBF16H
                   FH.allergies = c("C_25_2","SBF16G"),
                   FH.foodallergies = c("C_25_3","NO"),
                   FH.gastrointestinal = c("C_25_6","SBF16B"),
                   FH.cardiovascular = c("C_25_5","SBF16C"),
                   FH.infections = c("C_25_7","NO"),
                   FH.metabolism = c("C_25_8","SBF16I"),
                   FH.blood = c("NO","SBF16D"),
                   FH.autoimmune = c("NO","SBF16E"),
                   FH.nervoussystem = c("C_25_9","NO"),
                   FH.hudsykdommer = c("NO","SBF16F"),
                   FH.muscle_skeleton = c("C_2510","NO"),
                   FH.joints = c("C_2511","NO"),
                   FH.cancer = c("C_2512","SBF16L"),
                   FH.diabetes = c("C_2513","SBF16K"),
                   FH.other = c("C_2514","NO"),
                   FH.epillepsi = c("NO","SBF16J"),
                   FD.schizophrenia = c("C_26_1","SBF17A"),
                   FD.bipolar = c("C_26_2","SBF17B"),
                   FD.depression = c("C_26_3","SBF17C"),
                   FD.Anxiety = c("C_26_4","SBF17D"),
                   FD.ADHD = c("C_26_5","SBF17E"),
                   FD.OCD = c("C_26_6","SBF17F"),
                   FD.tourette = c("C_26_7","SBF17G"),
                   FD.autism = c("C_26_8","SBF17H"),              # merge?
                   FD.aspergers = c("C_26_9","SBF17H"),         # merge?
                   FD.motor = c("C_2610","SBF17L"),
                   FD.language = c("C_2611","SBF17K"),          
                   FD.learning = c("C_2612","SBF17M"),
                   FD.alcohol_drugs = c("C_2613","SBF17I"),
                   FD.mental_disability = c("C_2614","SBF17O"),
                   FD.rec_educational_support = c("C_2615","SBF17N"),
                   FD.ODD_CD = c("C_2615","SBF17J"),
                   FD.late_speech = c("C_2617","NO"),
                   FD.considered_eccentric = c("C_2618","NO"),
                   FD.other1 = c("C_2619","NO"),
                   FD.other2 = c("C_2620","NO"))
  
  pqb_levels = list( No = 1, donotknow = 2, sibling = 3, mother = 4, father = 5, mothers_family = 99, fathers_family = 99)
  pqa_levels = list( No = 1, sibling = 2, mother = 3, mothers_family = 4, father = 5,fathers_family = 6, donotknow = 7)
  
  new_levels = c(No = 0, donotknow = 1, sibling = 4, mother = 3, father = 3,  mothers_family = 2,  fathers_family = 2) 
  disorders = data.table(matrix(ncol = length(diagnosis)*7,nrow = dim(pqa)[1]+dim(pqb)[1]))
  setnames(disorders,
           names(disorders),
           unlist(lapply(names(diagnosis),function(x) paste(x,names(pqa_levels),sep = "."))))
  
  do = data.table(matrix(ncol = length(diagnosis),nrow = dim(pqa)[1]+dim(pqb)[1]))
  setnames(do,
           names(do),
           names(diagnosis))

  for (d in names(diagnosis)) {
    x = c()
    for (a in names(pqa_levels)[-1]){
      d_pqa = pqa[[paste(diagnosis[[d]][2],pqa_levels[[a]],sep = "_")]]
      d_pqb = pqb[[paste(diagnosis[[d]][1],pqb_levels[[a]],sep = "_")]]
      if (is.null(d_pqa)) d_pqa = rep(NA,548)
      if (is.null(d_pqb)) d_pqb = rep(NA,654)
      tmp = c(d_pqa,d_pqb)*new_levels[a]
      tmp[is.na(tmp)] = 0
      x = cbind(x,tmp)
    }
    
    x = apply(x,1, function(x){
      if (max(x) == 0) {
        0
      } else if (max(x) == 1) {
        1
      } else if (max(x) == 2 & sum(x == 2) == 1) {
        2
      } else if (max(x) == 2 & sum(x == 2) == 2) {
        3
      } else if (max(x) == 3 & sum(x == 3) == 1) {
        4
      } else if (max(x) == 3 & sum(x == 3) == 2) {
        5
      } else if (max(x) == 4) {
        6
      }
    })
    
    do[[d]] = labelled(x,labels = c("No" = 0, 
                                    "Don't know" = 1,
                                    "mothers or fathers family" = 2,
                                    "mothers and fathers family" = 3,
                                    "mother or father" = 4,
                                    "mother and father" = 5,
                                    "sibling" = 6))
  }
  
  
  do = cbind(rbind(pqa[,index_vars,with = F],
                   pqb[,index_vars,with = F]),
                    do)
  abbreviations = c(FH = "Health problems in family",
                     FD = "Disorder in family")
  do = add_label(do,"FH",abbreviations,my_warning = F)
  do = add_label(do,"FD",abbreviations,my_warning = F)
  rm(disorders)
  return(do)
}
