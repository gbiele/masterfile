#info: http://www.tandfonline.com/doi/pdf/10.1080/09297041003679344

get_brief = function(qu_a,qu_b,rater){
  
  if (rater == "P") {
    briefavars = c(index_vars,names(qu_a)[grep("SBFBRIEF",names(qu_a))])
    briefbvars = c(index_vars,names(qu_b)[grep("C_29",names(qu_b))])
    base_name = "BRF.P."
  } else if (rater == "T") {
    briefavars = c(index_vars,names(qu_a)[grep("BHBRIEF",names(qu_a))])
    briefbvars = c(index_vars,names(qu_b)[grep("B__3",names(qu_b))])
    base_name = "BRF.T."
  }
  
  briefa = qu_a[,briefavars,with = F]
  briefb = qu_b[,briefbvars,with = F]
  
  for (v in briefbvars[-c(1,2)]) {
    briefb[[v]] = briefb[[v]]-1
    attributes(briefb[[v]]) = attributes(briefa[[grep(v,names(briefb))]])
    }
  
 

  brief = rbind(briefa,briefb,use.names = F)
  rm(briefa,briefb,briefavars,briefbvars)
  
  brief_dims = data.table(read.csv2("instrument_docs/brief_items4dims.txt",sep = ","))
  setkeyv(brief_dims,"item")
  
  setnames(brief,names(brief)[-c(1,2)],paste0(base_name,brief_dims[,dimension],".i",brief_dims[,item]))
  
  for (d in unique(brief_dims$dimension)){
    items = names(brief)[grep(paste0(".",d,"."),names(brief))]
    brief = make_sum_scores(brief,items, paste0(base_name,d,".SS"))
  }
 
  abbreviations = c(BRF = "BRIEF (Behavior Rating Inventory of Executive Function)",
                    T = "teacher",
                    P = "parent",
                    SS = "sum of scores",
                    SC = "count of non-zero scores",
                    WM = "working memory",
                    PO = "planning and organizing",
                    EC = "Emotional control",
                    IH = "Inhibition",
                    SH = "Shift")
  brief = add_label(brief,"BRF",abbreviations)
  return(brief)
}
