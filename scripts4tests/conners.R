get_conners = function(qu,rater){
  
  ########### This code first constructs sum scores for
  ########### Domains that match the typical DSM groups
  ########### The conners was not intended for that
  ########### therefor these variables start with x as in xAIA
  ########### Below comes the classification as intended 
  
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
    Oitem2dims = list(OPP = c(2,6,11,16,20,24),
                      COG = c(3,8,12,17,21,25),
                      HYP = c(4,9,14,18,22,26),
                      ADHDI = c(1,5,7,10,13,15,17,19,21,23,25,27))
    items2dims = list(AIA = c(1, 4, 12, 14, 16, 17, 22, 23),
                      AHY = c(3,  7,  8, 10, 11, 19, 21, 25),
                      AIM = c(15, 24),
                      ODD = c(2,6,13),
                      CD = c(5,9,18,20))
  } else if (rater == "T") {
    CS = qu[,c(index_vars,names(qu)[grep("B__4",names(qu))]),with = F]
    base = "BHCONNERS_B"
    nbase = "CS.T."
    Oitem2dims = list(OPP = c(2,6,9,13,18),
                      COG = c(4,16),
                      HYP = c(3,7,10,15,19,21,24),
                      ADHDI = c(1,5,8,11,12,14,17,22,22,23,24,25))
    items2dims = list(AIA = c(1,3,5,8,10,12,15,19,21,27 ),
                      AHY = c(14,18,23),
                      AIM = c(4,7,9),
                      ODD = c(2,6,13,17,20,26),
                      CD = c(11,16,24))
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
  
  CS = CS[,c(index_vars,names(CS)[grep(".SS$",names(CS))]),with = F]
  setnames(CS,names(CS),gsub(nbase,paste0(nbase,"x"),names(CS)))
  
  
  ########## original Connsers scores 
  if (rater == "P") {
     Oitem2dims = list(OPP = c(2,6,11,16,20,24),
                      COG = c(3,8,12,17,21,25),
                      HYP = c(4,9,14,18,22,26),
                      ADHDI = c(1,5,7,10,13,15,17,19,21,23,25,27))
  } else if (rater == "T") {
    Oitem2dims = list(OPP = c(2,6,9,13,18),
                      COG = c(4,16),
                      HYP = c(3,7,10,15,19,21,24),
                      ADHDI = c(1,5,8,11,12,14,17,20,22,23,24,25))
  }
  CS = merge(CS,qu[,c(index_vars,names(qu)[grep(base,names(qu))]),with = F],by = index_vars)
  for (v in names(Oitem2dims)) {
    CS = make_sum_scores(CS,paste0(base,Oitem2dims[[v]]),paste0(nbase,v,".SS"))
  }
  
  for (k in 1:length(unique(unlist(Oitem2dims)))){
    old_name =  paste0(base,k)
    new_name = paste0(nbase,
                      gsub("[0-9]","",paste0(names(which((unlist(Oitem2dims) == k))),collapse = "")),
                      ".i",
                      k)
    setnames(CS,old_name,new_name)
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
                                        paste0(item2dims[[sub("x","",v)]],
                                               collapse = ","),
                                        ")")
  }
  
  return(CS)
}

