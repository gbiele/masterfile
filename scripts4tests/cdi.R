
############ CDI ################
# info: http://www.childdevrev.com/page11/page59/files/cdi-manual.pdf
# the coding in the spss syntax does not seem to be consistent with the official coding
# focus of the items seems to be on language

get_cdi = function(qu_a,qu_b,rater){
  
  if (rater == "P") {
    a_var = "SBFCDI"
    b_var = "C_27"
    base_name = "CDI.L.PA"
  } else if (rater == "T") {
    a_var = "BHCDI"
    b_var = "B__2"
    base_name = "CDI.L.TE" 
  }
  
  cdiavars = c(index_vars,names(qu_a)[grep(a_var,names(qu_a))])
  cdia = qu_a[,cdiavars,with = F]
  
  cdibvars = c(index_vars,names(qu_b)[grep(b_var,names(qu_b))])
  cdib = qu_b[,cdibvars,with = F]
  for (v in cdibvars[-c(1,2)]) cdib[[v]] = cdib[[v]]-1
  
  varlabels = c()
  for (k in 3:length(cdiavars)) {
    varlabels = sub("^ ","",c(varlabels,strsplit(attributes(cdia[[cdiavars[k]]])$label,";")[[1]][3]))
    eval(parse(text = paste("cdia[,",cdiavars[k]," := labelled(",cdiavars[k],",labels = c(No = 0, Yes = 1))]")))
    eval(parse(text = paste("cdib[,",cdibvars[k]," := labelled(",cdibvars[k],",labels = c(No = 0, Yes = 1))]")))
  }
  
  cdi = rbindlist(list(cdia,cdib),use.names = F)
  # set items with score -1 to NA
  # score of -1 must be a coding error
  cdi[cdi == -1] = NA
  
  setnames(cdi,names(cdi)[-c(1,2)], paste0("CDI.L.P.i",c(1:50,"pronounciation","understood")))
  
  cdi = smart_impute(cdi)
  
  cdi = make_sum_scores(cdi,names(cdi)[grep("CDI\\.L\\.P\\.i[0-9]",names(cdi))],"CDI.L.P.SS")
 
  for (k in 3:length(cdiavars)) attributes(cdi[[k]])$label = varlabels[k]
  
  abbreviations = c(CDI = "Child Development Inventory",
                    T = "teacher ratings",
                    P = "parent ratings",
                    SS = "sum of scores",
                    SC = "count of non-zero symptoms/scores",
                    L = "Language")
  cdi = add_label(cdi,"CDI",abbreviations)
  return(cdi)
}


get_cdi_kg = function(kgqa,kgqb){
  
  cdiavars = c(index_vars,names(kgqa)[grep("BHCDI",names(kgqa))])
  cdia = kgqa[,cdiavars,with = F]
  
  cdibvars = c(index_vars,names(kgqb)[grep("B__2",names(kgqb))])
  cdib = kgqb[,cdibvars,with = F]
  for (v in cdibvars[3:54]) cdib[[v]] = cdib[[v]]-1

  varlabels = c()
  for (k in 3:length(cdiavars)) {
    varlabels = sub(" . ","",c(varlabels,strsplit(attributes(cdia[[cdiavars[k]]])$label,";")[[1]][2]))
    eval(parse(text = paste("cdia[,",cdiavars[k]," := labelled(",cdiavars[k],",labels = c(No = 0, Yes = 1))]")))
    eval(parse(text = paste("cdib[,",cdibvars[k]," := labelled(",cdibvars[k],",labels = c(No = 0, Yes = 1))]")))
  }
  
  cdi = rbind(cdia,cdib,use.names = F)
  # set items with score -1 to NA
  # score of -1 must be a coding error
  cdi[cdi == -1] = NA
  
  setnames(cdi,names(cdi)[-c(1,2)], paste0("CDI.L.T.i",c(1:50,"pronounciation","understood")))
  
  cdi = make_sum_scores(cdi,names(cdi)[grep("CDI\\.L\\.T\\.i[0-9]",names(cdi))],"CDI.L.T.SS")
  
  for (k in 3:length(cdiavars)) attributes(cdi[[k]])$label = varlabels[k]
  
  abbreviations = c(CDI = "CDI",
                    T = "teacher ratings",
                    P = "parent ratings",
                    SS = "sum of scores",
                    SC = "count of non-zero symptoms/scores",
                    L = "Language")
  cdi = add_label(cdi,"CDI",abbreviations)
  return(cdi)
}
