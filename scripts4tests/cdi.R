


############ CDI ################
# info: http://www.childdevrev.com/page11/page59/files/cdi-manual.pdf
# the coding in the spss syntax does not seem to be consistent with the official coding
# focus of the items seems to be on language

get_cdi = function(qu_a,qu_b,rater){
  
  if (rater == "parent") {
    a_var = "SBFCDI"
    b_var = "C_27"
    base_name = "CDIlang.parent"
  } else if (rater == "teacher") {
    a_var = "BHCDI"
    b_var = "B__2"
    base_name = "CDIlang.teacher" 
  }
  
  cdiavars = c("PREG_ID_299","BARN_NR",names(qu_a)[grep(a_var,names(qu_a))])
  cdia = qu_a[,cdiavars,with = F]
  
  cdibvars = c("PREG_ID_299","BARN_NR",names(qu_b)[grep(b_var,names(qu_b))])
  cdib = qu_b[,cdibvars,with = F]
  
  cdi = rbind(cdia,cdib,use.names = F)
  
  setnames(cdi,names(cdi)[-c(1,2)], paste(base_name,c(1:50,".pronounciation",".understood"),sep = ".item"))
  
  cdi[[paste(base_name,"sum",sep =".")]] = rowSums(cdi[,paste(base_name,1:50,sep = ".item"),with = F],na.rm = T)
  cdi[[paste(base_name,"mean",sep =".")]] = rowMeans(cdi[,paste(base_name,1:50,sep = ".item"),with = F],na.rm = T)
  cdi[[paste(base_name,"missings",sep =".")]] = rowSums(is.na(cdi[,paste(base_name,1:50,sep = ".item"),with = F]))
  return(cdi)
}


get_cdi_kg = function(kgqa,kgqb){
  
  cdiavars = c("PREG_ID_299","BARN_NR",names(kgqa)[grep("BHCDI",names(kgqa))])
  cdia = kgqa[,cdiavars,with = F]
  
  cdibvars = c("PREG_ID_299","BARN_NR",names(kgqb)[grep("B__2",names(kgqb))])
  cdib = kgqb[,cdibvars,with = F]
  cdib = cbind(cdib[,1:2,with = F],cdib[,3:54,with = F] - 1)
  
  cdi = rbind(cdia,cdib,use.names = F)
  
  setnames(cdi,names(cdi)[-c(1,2)], paste("CDIlang.teacher.item",c(1:50,"pronounciation","understood"),sep = "."))
  
  cdi$CDIlang.teacher.score.sum = rowSums(cdi[,paste("CDIlang.teacher.item",1:50,sep = "."),with = F],na.rm = T)
  cdi$CDIlang.teacher.score.mean = rowMeans(cdi[,paste("CDIlang.teacher.item",1:50,sep = "."),with = F],na.rm = T)
  cdi$CDIlang.teacher.missings = rowSums(is.na(cdi[,paste("CDIlang.teacher.item",1:50,sep = "."),with = F]))
  return(cdi)
}
