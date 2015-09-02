get_ADHD_SCALE_Q6 = function(){
  dt = data.table(read_sav("savs/Atferd.sav"))
  
  old_names = names(dt)[grep("^AT3",names(dt))]
  new_names = paste("A.MB3.I",1:11,sep = "")
  setnames(dt,old_names,new_names)
  dt = make_sum_scores(dt,new_names,"A.MB3.SS")
  dt[,A.MB3.SS := A.MB3.SS-min(A.MB3.SS)]
  
  
  dt[,A.MB3.SC := round(mean(.SD > 1,na.rm = T)*11),by = 1:nrow(dt),.SDcols = new_names]
  dt[,nNA := sum(is.na(.SD)),by = 1:nrow(dt),.SDcols = new_names]
  dt[nNA >= (length(new_names)/2), A.MB3.SC := NA]
  dt = dt[,-grep("nNA",names(dt)),with = F]
  
  abbreviations = c(A = "ADHD",
                    I = "Item",
                    SS = "sum of scores",
                    SC = "symptom count")
  
  
  return(dt[,c("PREG_ID_299","BARN_NR",new_names,"A.MB3.SS"),with = F])
}