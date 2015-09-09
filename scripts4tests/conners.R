get_conners = function(qu,rater){
  
  i2d = fread("instrument_docs/CONNERS.csv")
  i2d[,question := gsub("[0-9]","",question)]
  dims = unique(i2d$dimension)
  item2dims = vector(mode = "list",length = length( dims) )
  for (v in dims) {
    item2dims[[which(v == dims)]] =  i2d[which(rater == "T" & dimension == v),inumber]
  }
  names(item2dims) = dims
  
  if (rater == "P") {
    CS = qu[,c(index_vars,names(qu)[grep("C_30",names(qu))]),with = F]
    base = "SBFCONNERS"
    nbase = "CS.P."
  } else if (rater == "T") {
    CS = qu[,c(index_vars,names(qu)[grep("B__4",names(qu))]),with = F]
    base = "BHCONNERS_B"
    nbase = "CS.T."
  }
  
  CS = qu[,c(index_vars,names(qu)[grep(base,names(qu))]),with = F]
  
  for (v in dims) {
    old_names = paste0(base,item2dims[[v]])

    new_names = paste0(nbase,v,".i",item2dims[[v]])
    setnames(CS,old_names,new_names)
    CS = make_sum_scores(CS,new_names,paste0(nbase,v,".SS"))
  }
  CS = make_sum_scores(CS,
                        unlist(lapply(names(item2dims[2:3]), 
                                      function(v) paste0(nbase,v,".i",item2dims[[v]]))),
                        paste0(nbase,"AHI.SS"))
  CS = make_sum_scores(CS,
                        unlist(lapply(names(item2dims[1:3]), 
                                      function(v) paste0(nbase,v,".i",item2dims[[v]]))),
                        paste0(nbase,"ADHD.SS"))
  
  abbreviations = c(ADHD = "Attention deficit hyperactivity disorder",
                    CS = "CTRS R-s (Conners' teache rating scale revised, short, preschol)",
                    T = "Teacher ratings",
                    P = "Parent ratings",
                    SS = "sum of scores",
                    AIA = "ADHD inattention",
                    AHY = "ADHD hyperactivity",
                    AHI = "ADHD Hyperactivity & Impulsivity",
                    AIM = "ADHD impulsivity",
                    CD = "Conduct disorder",
                    ODD = "Oppositional defiant disorder")
  
  CS = CS[,c(index_vars,names(CS)[grep("^CS",names(CS))]),with = F]
  
  CS = add_label(CS,"CS",abbreviations)
}

