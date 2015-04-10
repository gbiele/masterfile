


############ CDI ################
# info: http://www.childdevrev.com/page11/page59/files/cdi-manual.pdf
# the coding in the spss syntax does not seem to be consistent with the official coding
# focus of the items seems to be on language

get_cdi = function(pqa,pqb){
  
  cdiavars = c("PREG_ID_299","BARN_NR",names(pqa)[grep("SBFCDI",names(pqa))])
  cdia = pqa[,cdiavars,with = F]
  
  cdibvars = c("PREG_ID_299","BARN_NR",names(pqb)[grep("C_27",names(pqb))])
  cdib = pqb[,cdibvars,with = F]
  
  cdi = rbind(cdia,cdib,use.names = F)
  
  setnames(cdi,names(cdi)[-c(1,2)], paste("CDIlang.parent.item",c(1:50,"pronounciation","understood"),sep = "."))
  
  cdi$CDIlang.parent.score.sum = rowSums(cdi[,paste("CDIlang.parent.item",1:50,sep = "."),with = F],na.rm = T)
  cdi$CDIlang.parent.score.mean = rowMeans(cdi[,paste("CDIlang.parent.item",1:50,sep = "."),with = F],na.rm = T)*50
  cdi$CDIlang.parent.missings = rowSums(is.na(cdi[,paste("CDIlang.parent.item",1:50,sep = "."),with = F]))
  return(cdi)
}
