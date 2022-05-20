get_ADHD_SCALE_Q6 = function(){
  data_dir = ifelse(
    file.exists(paste0(data_dir,"StB.sav")),
    data_dir,
    paste0(data_dir,"PDB2565_"))
  dt = NaN2NA(data.table(read_sav(paste0(data_dir,"ATFERD.sav"))))
  dt = merge(dt,
             dt[,list(N = .N), by = index_vars],
             by = index_vars)
  
  dt[, delete := (N > 1 & is.na(AlderUtfylt_ATFERD))]
  dt = dt[delete == F]
  
  old_names = names(dt)[grep("^AT3",names(dt))]
  new_names = paste("MB3S.ADHDi",1:11,sep = "")
  setnames(dt,old_names, new_names)
  for (i in new_names) dt[[i]][ dt[[i]] > 3 | dt[[i]] < 1 ] = NA
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

