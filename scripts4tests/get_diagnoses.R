
get_diagnoses = function(pqa,pqb){
  diagnosis = list(skin = c("C_24_1_1","SBF15B_1"),
                   nose_mouth_throat = c("C_24_2_1","NO"),
                   asthma_lung = c("C_24_3_1","SBF15C_1"), # and SBF15D_1
                   allergies = c("C_24_4_1","SBF15E_1"),
                   foodallergies = c("C_24_5_1","SBF15F_1"),
                   cardiovascular = c("C_24_6_1","SBF15G_1"),
                   blood = c("C_24_7_1","SBF15H_1"),
                   gastrointestinal = c("C_24_8_1","SBF15I_1"),
                   seriousinfection = c("C_24_9_1","SBF15J_1"),
                   metabolism = c("C_2410_1","SBF15K_1"),
                   nervoussystem = c("C_2411_1","SBF15L_1"),
                   muscle_skeleton = c("C_2412_1","SBF15M_1"),
                   joints = c("C_2413_1","SBF15N_1"),
                   eyesight = c("C_2414_1","SBF15O_1"),
                   hearing = c("C_2415_1","SBF15P_1"),
                   cancer = c("C_2416_1","SBF15Q_1"),
                   Congenital_deformity = c("C_2417_1","SBF15R_1"),
                   genetic_disease = c("C_2418_1","NO"),
                   diabetes = c("C_2419_1","SBF15S_1"),
                   recurrent_fever = c("C_2420_1","SBF15T_1"),
                   genital_urinary = c("C_2421_1","NO"),
                   other = c("C_2422_1","SBF15U_1"))
  diags = data.table(matrix(ncol = length(diagnosis),nrow = dim(pqa)[1]+dim(pqb)[1]))
  setnames(diags,names(diags),names(diagnosis))
  pqa[["NO"]] = NA
  pqa[["SBF15C_1"]] = rowSums(cbind(as.numeric(pqa$SBF15C_1 == 1),as.numeric(pqa$SBF15D_1 == 1)),na.rm = T) > 0
  for (d in names(diagnosis)) {
    tmp = c(pqa[[diagnosis[[d]][2]]],pqb[[diagnosis[[d]][1]]]-1)
    tmp[tmp<0] = 0
    diags[[d]] = factor(tmp,labels = c("No","Yes")[1:length(unique(tmp[!is.na(tmp)]))],ordered = F)
  }
  setnames(diags,names(diags),paste("CHILD_DIAGNOSIS.",names(diags),sep = ""))
  diags = cbind(rbind(pqa[,list(PREG_ID_299,BARN_NR)],pqb[,list(PREG_ID_299,BARN_NR)]),
                diags)
  return(diags)
}
