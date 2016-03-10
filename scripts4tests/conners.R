get_conners = function(qu,rater){
  
  ########### This code first constructs sum scores for
  ########### Domains that match the typical DSM groups
  ########### The conners was not intended for that
  ########### therefor these variables start with x as in xAIA
  ########### Below comes the classification as intended 
  
  if (rater == "P") {
    CS = qu[,c(index_vars,names(qu)[grep("C_30",names(qu))]),with = F]
    base = "SBFCONNERS"
    nbase = "CS.P."
    Oitems2dims = list(OPP = c(2,6,11,16,20,24),
                       COG = c(3,8,12,17,21,25),
                       HYP = c(4,9,14,18,22,26),
                       ADHDI = c(1,5,7,10,13,15,17,19,21,23,25,27))
    items2dims = list(xAIA = c(1,3,5,8,10,12,13,15, 19, 21, 25, 27),
                      xAHY = c(4, 7, 14, 18, 23, 26),
                      xAIM = c(9, 22),
                      xODD = c(2,6,17,20),
                      xCD = c(11,24))
  } else if (rater == "T") {
    CS = qu[,c(index_vars,names(qu)[grep("B__4",names(qu))]),with = F]
    base = "BHCONNERS_B"
    nbase = "CS.T."
    Oitems2dims = list(OPP = c(2,6,9,13,18),
                       COG = c(4,16),
                       HYP = c(3,7,10,15,19,21,24),
                       ADHDI = c(1,5,8,11,12,14,17,20,22,23,24,25))
    items2dims = list(xAIA = c(1,4,12,14,16,17,22,23),
                      xAHY = c(3,7,8,10,11,19,21,25),
                      xAIM = c(15,24),
                      xODD = c(2,6,13),
                      xCD = c(5,9,18,20))
  }
  
  CS = qu[,c(index_vars,names(qu)[grep(base,names(qu))]),with = F]
  
  for (v in names(items2dims)) {
    old_names = paste0(base,items2dims[[v]])
    
    new_names = paste0(nbase,v,".i",items2dims[[v]])
    setnames(CS,old_names,new_names)
    CS = make_sum_scores(CS,new_names,paste0(nbase,v,".SS"),
                         count_cutoff_idx = 2)
  }
  CS = make_sum_scores(CS,
                       unlist(lapply(names(items2dims[2:3]), 
                                     function(v) paste0(nbase,v,".i",items2dims[[v]]))),
                       paste0(nbase,"xAHI.SS"),
                       count_cutoff_idx = 2)
  CS = make_sum_scores(CS,
                       unlist(lapply(names(items2dims[1:3]), 
                                     function(v) paste0(nbase,v,".i",items2dims[[v]]))),
                       paste0(nbase,"xADHD.SS"),
                       count_score = T,
                       count_cutoff_idx = 2)
  
  CS = CS[,c(index_vars,names(CS)[grep(".SS$|.SC$",names(CS))]),with = F]
  
  
  
  CS = merge(CS,qu[,c(index_vars,names(qu)[grep(base,names(qu))]),with = F],by = index_vars, all = T)
  for (v in names(Oitems2dims)) {
    CS = make_sum_scores(CS,
                         paste0(base,Oitems2dims[[v]]),
                         paste0(nbase,v,".SS"),
                         count_score = T,
                         count_cutoff_idx = 2)
  }
  
  for (k in 1:length(unique(unlist(Oitems2dims)))){
    old_name =  paste0(base,k)
    new_name = paste0(nbase,
                      gsub("[0-9]","",paste0(names(which((unlist(Oitems2dims) == k))),collapse = "")),
                      ".i",
                      k)
    setnames(CS,old_name,new_name)
  }
  
  ################# scale symtom sum and count scores #########
  orig_length = list(xAIA = 9, xAHY = 6, xAIM = 3, xODD = 8, xCD = 8, xADHD = 18, xAHI = 9)
  current_lengths = list(xADHD = 18, xAHI = 9)
  for (v in names(orig_length)) {
    for (sv in c("SC","SS")) {
      vname = paste("CS",rater,v,sv,sep = ".")
      current_length = ifelse(length(intersect(v,names(items2dims))) > 0,
                              length(items2dims[[v]]),
                              current_lengths[[v]])
      if (length(CS[[vname]]) > 0)
        CS[[vname]] = round((CS[[vname]]/current_length)*orig_length[[v]])
    }
  }
  
  
  
  abbreviations = c(ADHD = "ADHD",
                    CS = "CTRS_Rs",
                    T = "Teacher ratings",
                    P = "Parent ratings",
                    SS = "sum of scores",
                    SC = "count of non-zero scores",
                    xAIA = "ADHD inattention (!= Conners!)",
                    xAHY = "ADHD hyperactivity (!= Conners!)",
                    xAHI = "ADHD Hyperactivity & Impulsivity (!= Conners!)",
                    xAIM = "ADHD impulsivity (!= Conners!)",
                    xCD = "Conduct disorder (!= Conners!)",
                    xADHD = "ADHD (!= Conners!)",
                    xODD = "ODD (!= Conners!)",
                    OPP = "A: Oppositional",
                    COG = "B: Cognitive problems/Inattention",
                    HYP = "C: Hyperactivity",
                    ADHDI = "D: ADHD index",
                    COGADHDI = "Cognitive problems/Inattention & ADHD index")
  
  CS = add_label(CS,"CS",abbreviations)
  
  for (v in c("xAHY","xAIA","xAIM","xCD","xODD")){
    vn = paste0(nbase,v,".SS")
    attributes(CS[[vn]])$label = paste0(attr(CS[[vn]],"label"),
                                        "; (Items: ",
                                        paste0(items2dims[[sub("x","",v)]],
                                               collapse = ","),
                                        ")")
  }
  
  return(CS)
}

