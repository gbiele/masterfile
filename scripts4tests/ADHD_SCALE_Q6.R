get_ADHD_SCALE_Q6 = function(){
  dt = data.table(read_sav("savs/Atferd.sav"))
  
  old_names = names(dt)[grep("^AT3",names(dt))]
  new_names = paste("ADHD_screening_scale.item",1:11,sep = "")
  setnames(dt,old_names,new_names)
  dt$ADHD_screening_scale.sumSCORE = make_sum_scores(dt[,new_names,with = F])
  dt[,ADHD_screening_scale.sumSCORE := ADHD_screening_scale.sumSCORE-min(ADHD_screening_scale.sumSCORE)]
  return(dt[,c("PREG_ID_299","BARN_NR",new_names,"ADHD_screening_scale.sumSCORE"),with = F])
}