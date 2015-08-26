
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
  for (v in cdibvars[-c(1,2)]) cdib[[v]] = cdib[[v]]-1
  
  varlabels = c()
  for (k in 3:length(cdiavars)) {
    varlabels = sub("^ ","",c(varlabels,strsplit(attributes(cdia[[cdiavars[k]]])$label,";")[[1]][2]))
    eval(parse(text = paste("cdia[,",cdiavars[k]," := labelled(",cdiavars[k],",labels = c(No = 0, Yes = 1))]")))
    eval(parse(text = paste("cdib[,",cdibvars[k]," := labelled(",cdibvars[k],",labels = c(No = 0, Yes = 1))]")))
  }
  
  cdi = rbindlist(list(cdia,cdib),use.names = F)
  
  setnames(cdi,names(cdi)[-c(1,2)], paste("CDIlang.PA",c(1:50,"pronounciation","understood"),sep = "."))
  
  cdi = make_sum_scores(cdi,names(cdi)[grep("PA[0-9]",names(cdi))],"CDIlang.PA.SS")
 
  for (k in 3:length(cdiavars)) attributes(cdi[[k]])$label = varlabels[k]
  
  return(cdi)
}


get_cdi_kg = function(kgqa,kgqb){
  
  cdiavars = c("PREG_ID_299","BARN_NR",names(kgqa)[grep("BHCDI",names(kgqa))])
  cdia = kgqa[,cdiavars,with = F]
  
  cdibvars = c("PREG_ID_299","BARN_NR",names(kgqb)[grep("B__2",names(kgqb))])
  cdib = kgqb[,cdibvars,with = F]
  cdib = cbind(cdib[,1:2,with = F],cdib[,3:54,with = F] - 1)
  
  varlabels = c()
  for (k in 3:length(cdiavars)) {
    varlabels = sub(" . ","",c(varlabels,strsplit(attributes(cdia[[cdiavars[k]]])$label,";")[[1]][3]))
    eval(parse(text = paste("cdia[,",cdiavars[k]," := labelled(",cdiavars[k],",labels = c(No = 0, Yes = 1))]")))
    eval(parse(text = paste("cdib[,",cdibvars[k]," := labelled(",cdibvars[k],",labels = c(No = 0, Yes = 1))]")))
  }
  
  cdi = rbind(cdia,cdib,use.names = F)
  
  setnames(cdi,names(cdi)[-c(1,2)], paste("CDIlang.TE",c(1:50,"pronounciation","understood"),sep = "."))
  
  cdi = make_sum_scores(cdi,names(cdi)[grep("TE[0-9]",names(cdi))],"CDIlang.TE.SS")
  
  for (k in 3:length(cdiavars)) attributes(cdi[[k]])$label = varlabels[k]
  
  return(cdi)
}
