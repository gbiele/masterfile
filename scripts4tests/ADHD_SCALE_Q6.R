get_ADHD_SCALE_Q6 = function(){
  dt = data.table(read_sav("savs/Atferd.sav"))
  
  old_names = names(dt)[grep("^AT3",names(dt))]
  new_names = paste("MB3S.ADHDi",1:11,sep = "")
  setnames(dt,old_names,new_names)
  dt = make_sum_scores(dt,new_names,"MB3S.ADHD.SS")
  abbreviations = c(ADHD = "ADHD",
                    MB3S = "scale from Moba questionnaire 6 (3 years)",
                    I = "Item",
                    SS = "sum of scores",
                    SC = "count of non-zero scores")
  dt = dt[,c(index_vars,new_names,"MB3S.ADHD.SS"),with = F]
  dt = add_label(dt,"MB3S",abbreviations)
  
  return(dt)
}

