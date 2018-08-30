get_conclusion = function() {
  concl = data.table(read_sav(paste0(data_dir,"ADHD_KONKL.sav")))
  setnames(concl,c("KU8_3_1","KU8_3_2","KU10_1_1"),c("C.rewards.S","C.breaks.S", "C.collaboration.S"))
  
  concl[,C.rewards.S := as.numeric(C.rewards.S)]
  attr(concl$C.rewards.S,"label") = "Number of reward during testing day"
  concl[,C.breaks.S := as.numeric(C.breaks.S)]
  attr(concl$C.breaks.S,"label") = "Number of breaks during testing day"
  concl[, C.collaboration.S := factor(C.collaboration.S, labels = c("normal", "somewhat reduced", "clearly reduced"))]
  attr(concl$C.collaboration.S,"label") = "Collaboration during testing day"
  return(concl[,c(index_vars,grep("^C\\.",names(concl), value = T)),with = F])
}

