#info: http://www.tandfonline.com/doi/pdf/10.1080/09297041003679344

get_brief = function(pqa,pqb){
  briefavars = c("PREG_ID_299","BARN_NR",names(pqa)[grep("SBFBRIEF",names(pqa))])
  briefa = pqa[,briefavars,with = F]
  
  briefbvars = c("PREG_ID_299","BARN_NR",names(pqb)[grep("C_29",names(pqb))])
  briefb = pqb[,briefbvars,with = F]
  
  brief = rbind(briefa,briefb,use.names = F)
  rm(briefa,briefb,briefavars,briefbvars)
  
  return(brief)
}