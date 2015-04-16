#info: http://www.tandfonline.com/doi/pdf/10.1080/09297041003679344

get_brief = function(qu_a,qu_b,rater){
  
  if (rater == "parent") {
    briefavars = c("PREG_ID_299","BARN_NR",names(qu_a)[grep("SBFBRIEF",names(qu_a))])
    briefbvars = c("PREG_ID_299","BARN_NR",names(qu_b)[grep("C_29",names(qu_b))])
    base_name = "BRIEF.parent."
  } else if (rater == "teacher") {
    briefavars = c("PREG_ID_299","BARN_NR",names(qu_a)[grep("BHBRIEF",names(qu_a))])
    briefbvars = c("PREG_ID_299","BARN_NR",names(qu_b)[grep("B__3",names(qu_b))])
    base_name = "BRIEF.teacher."
  }
  
  
  briefa = qu_a[,briefavars,with = F]
  briefb = qu_b[,briefbvars,with = F]-1
  
  brief = rbind(briefa,briefb,use.names = F)
  rm(briefa,briefb,briefavars,briefbvars)
  
  setnames(brief,names(brief)[-c(1,2)],paste(base_name,1:63,sep = "item"))
  
  brief_dims = data.table(read.csv2("intrument_docs//brief_items4dims.txt",sep = ","))
  
  for (d in unique(brief_dims$dimension)){
    brief[[paste(base_name,d,sep = "score.")]] = rowSums(brief[,paste(base_name,brief_dims[dimension == d,item],sep = "item"),with = F])
  }
 
  return(brief)
}