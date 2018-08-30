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
    briefb[[v]][briefb[[v]]<1] = NA
    briefb[[v]] = briefb[[v]]-1
    attributes(briefb[[v]]) = attributes(briefa[[grep(v,names(briefb))]])
    }
  
 

  brief = rbind(briefa,briefb,use.names = F)
  rm(briefa,briefb,briefavars,briefbvars)
  brief_dims = data.table(read.csv2("instrument_docs/brief_items4dims.txt",sep = ","))
  setkeyv(brief_dims,"item")
  
  setnames(brief,names(brief)[-c(1,2)],paste0(base_name,brief_dims[,dimension],".i",sprintf("%02d",brief_dims[,item])))
  brief = smart_impute(brief)
  
  if(rater == "P") iteminfo = ""
  for (d in unique(brief_dims$dimension)){
    items = names(brief)[grep(paste0(".",d,"."),names(brief))]
    brief = make_sum_scores(brief,items, paste0(base_name,d,".SS"))
    if(rater == "P") {iteminfo = paste(iteminfo,
                                       d,
                                       "\n",
                                       paste(sapply(brief[,items,with = F], attr, "label"),collapse = "\n"),
                                       "\n")}
  }
  
  if(rater == "P") cat(iteminfo,file = "item_info/brief.txt")
  
  # brief$x.GEC.SS = rowSums(brief[,grep("SS",names(brief)), with = F])
  # brief$x.ISCI.SS = rowSums(brief[,grep("IH\\.SS$|EC\\.SS$",names(brief)), with = F])
  # brief$x.FI.SS = rowSums(brief[,grep("SH\\.SS$|EC\\.SS$",names(brief)), with = F])
  # brief$x.EMO.SS = rowSums(brief[,grep("WM\\.SS$|PO\\.SS$",names(brief)), with = F])
  # neg_items = c(14,28,30,34,39,41,58,60)
  # brief$x.NE.SS = rowSums(brief[,grep(paste0("i",neg_items,"$", collapse = "|"),names(brief),value = T), with = F])
  abbreviations = c(BRF = "BRIEF-Preschool",
                    T = "teacher",
                    P = "parent",
                    SS = "sum of scores",
                    SC = "count of non-zero scores",
                    WM = "Working memory",
                    PO = "Planning and organizing",
                    EC = "Emotional control",
                    IH = "Inhibit",
                    SH = "Shift",
                    GEC = "Global Executive Composite",
                    ISCI = "Inhibitory Self-Control Index",
                    FI = "Flexibility Index",
                    EMI = "Emergent Metacognition Index",
                    NE = "Negativity (validation)",
                    INC = "Inconsistency (validation)")
  brief = add_label(brief,"BRF",abbreviations)
  return(brief)
}
