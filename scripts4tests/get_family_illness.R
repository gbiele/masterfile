get_family_illness = function(pqa,pqb){
  diagnosis = list(FAM_HEALTH.skin = c("C_25_1","NO"),
                   FAM_HEALTH.asthma_lung = c("C_25_2","SBF16A"), # and SBF16H
                   FAM_HEALTH.allergies = c("C_25_2","SBF16G"),
                   FAM_HEALTH.foodallergies = c("C_25_3","NO"),
                   FAM_HEALTH.gastrointestinal = c("C_25_6","SBF16B"),
                   FAM_HEALTH.cardiovascular = c("C_25_5","SBF16C"),
                   FAM_HEALTH.infections = c("C_25_7","NO"),
                   FAM_HEALTH.metabolism = c("C_25_8","SBF16I"),
                   FAM_HEALTH.blood = c("NO","SBF16D"),
                   FAM_HEALTH.autoimmune = c("NO","SBF16E"),
                   FAM_HEALTH.nervoussystem = c("C_25_9","NO"),
                   FAM_HEALTH.hudsykdommer = c("NO","SBF16F"),
                   FAM_HEALTH.muscle_skeleton = c("C_2510","NO"),
                   FAM_HEALTH.joints = c("C_2511","NO"),
                   FAM_HEALTH.cancer = c("C_2512","SBF16L"),
                   FAM_HEALTH.diabetes = c("C_2513","SBF16K"),
                   FAM_HEALTH.other = c("C_2514","NO"),
                   FAM_HEALTH.epillepsi = c("NO","SBF16J"),
                   FAM_DISORDERS.schizophrenia = c("C_26_1","SBF17A"),
                   FAM_DISORDERS.bipolar = c("C_26_2","SBF17B"),
                   FAM_DISORDERS.depression = c("C_26_3","SBF17C"),
                   FAM_DISORDERS.Anxiety = c("C_26_4","SBF17D"),
                   FAM_DISORDERS.ADHD = c("C_26_5","SBF17E"),
                   FAM_DISORDERS.OCD = c("C_26_6","SBF17F"),
                   FAM_DISORDERS.tourette = c("C_26_7","SBF17G"),
                   FAM_DISORDERS.autism = c("C_26_8","SBF17H"),              # merge?
                   FAM_DISORDERS.aspergers = c("C_26_9","SBF17H"),         # merge?
                   FAM_DISORDERS.motor = c("C_2610","SBF17L"),
                   FAM_DISORDERS.language = c("C_2611","SBF17K"),          
                   FAM_DISORDERS.learning = c("C_2612","SBF17M"),
                   FAM_DISORDERS.alcohol_drugs = c("C_2613","SBF17I"),
                   FAM_DISORDERS.mental_disability = c("C_2614","SBF17O"),
                   FAM_DISORDERS.rec_educational_support = c("C_2615","SBF17N"),
                   FAM_DISORDERS.ODD_CD = c("C_2615","SBF17J"),
                   FAM_DISORDERS.late_speech = c("C_2617","NO"),
                   FAM_DISORDERS.considered_eccentric = c("C_2618","NO"),
                   FAM_DISORDERS.other1 = c("C_2619","NO"),
                   FAM_DISORDERS.other2 = c("C_2620","NO"))
  
  pqb_levels = list( No = 1, donotknow = 2, yes_sibling = 3, yes_mother = 4, yes_father = 5, yes_mothers_family = 99, yes_fathers_family = 99)
  pqa_levels = list( No = 1, yes_sibling = 2, yes_mother = 3, yes_mothers_family = 4, yes_father = 5,yes_fathers_family = 6, donotknow = 7)
  
  disorders = data.table(matrix(ncol = length(diagnosis)*7,nrow = dim(pqa)[1]+dim(pqb)[1]))
  setnames(disorders,
           names(disorders),
           unlist(lapply(names(diagnosis),function(x) paste(x,names(pqa_levels),sep = "."))))
  

  for (d in names(diagnosis)) {
    for (a in names(pqa_levels)){
      d_pqa = pqa[[paste(diagnosis[[d]][2],pqa_levels[[a]],sep = "_")]]
      d_pqb = pqb[[paste(diagnosis[[d]][1],pqb_levels[[a]],sep = "_")]]
      if (is.null(d_pqa)) d_pqa = rep(NA,548)
      if (is.null(d_pqb)) d_pqb = rep(NA,654)
      disorders[[paste(d,a,sep = ".")]] = c(d_pqa,d_pqb)
    }
  }
  
  disorders[is.na(disorders)] = 0
  disorders = data.table(disorders == 1)
  
  disorders = cbind(rbind(pqa[,list(PREG_ID_299,BARN_NR)],pqb[,list(PREG_ID_299,BARN_NR)]),
                    disorders)
  return(disorders)
}
