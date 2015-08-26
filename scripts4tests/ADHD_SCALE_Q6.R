get_ADHD_SCALE_Q6 = function(){
  dt = data.table(read_sav("savs/Atferd.sav"))
  
  old_names = names(dt)[grep("^AT3",names(dt))]
  new_names = paste("ADHD.MOBAQ3.SCALE",1:11,sep = "")
  setnames(dt,old_names,new_names)
  dt = make_sum_scores(dt,new_names,"ADHD.MOBAQ3.SCALE.SS")
  dt[,ADHD.MOBAQ3.SCALE.SS := ADHD.MOBAQ3.SCALE.SS-min(ADHD.MOBAQ3.SCALE.SS)]
  return(dt[,c("PREG_ID_299","BARN_NR",new_names,"ADHD.MOBAQ3.SCALE.SS"),with = F])
}