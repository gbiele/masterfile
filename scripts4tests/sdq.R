get_sdq = function(qu_a,qu_b,rater){
  
  if (rater == "P") {
    sdqavars = c(index_vars,names(qu_a)[grep("SBFSDQ",names(qu_a))])
    sdqavars = setdiff(sdqavars,c("SBFSDQ26","SBFSDQ28","SBFSDQ34"))
    sdqbvars = c(index_vars,names(qu_b)[grep("C_31",names(qu_b))])
    sdqbvars = setdiff(sdqbvars,c("C_31_5","C_3111","C_3112"))
    base_name = "SDQ.P." 
    sbase = "SBFSDQ"
    impact = c("distres","imphome","imppeer","impclas","impleis")
    sdq_sav_stub = "C_31"
  } else if (rater == "T") {
    sdqavars = c(index_vars,names(qu_a)[grep("BHSDQ",names(qu_a))])
    sdqavars = setdiff(sdqavars,c("BHSDQ26","BHSDQ28","BHSDQ34"))
    sdqbvars = c(index_vars,names(qu_b)[grep("B__5",names(qu_b))])
    sdqbvars = setdiff(sdqbvars,c("B__5_5","B__5_9"))
    base_name = "SDQ.T." 
    sbase = "BHSDQ_B"
    impact = c("distres","imppeer","impclas")
    sdq_sav_stub = "B__5"
  }
  
  ################# SDQ #######################
  # scoring: http://www.sdqinfo.org/c9.html

  sdq_dims = list(EMOTION = c("somatic", "worries", "unhappy", "clingy", "afraid"),
                  CONDUCT = c("tantrum", "q.obeys", "fights", "lies", "steals"),
                  HYPERACT   = c("restles", "fidgety", "distrac", "q.reflect", "qattends"),
                  PEERREL    = c("loner", "q.friend", "q.popular", "bullied", "oldbest"),
                  PROSOCIAL  = c("consid", "shares", "caring", "kind", "helpout"),
                  IMPACT  = impact)
  items = c("consid", "restles","somatic","shares","tantrum","loner",
            "obeys","worries","caring","fidgety","friend","fights",
            "unhappy","popular","distrac","clingy","kind","lies",
            "bullied","helpout","reflect","steals","oldbest","afraid",
            "attends")
  
  refelcted_vars = unlist(sdq_dims)[grep("^q.",unlist(sdq_dims))]
  #rename variables sdqa
  sdqa = qu_a[,sdqavars,with = F]
  setnames(sdqa,paste0(sbase,1:25), paste0(base_name,items))
  setnames(sdqa,paste0(sbase,27),"ebddiff")
  setnames(sdqa,paste0(sbase,1:length(impact)+28),paste(base_name,impact,sep = "IPT."))
  
  # reflect variables sdqa
  for (v in refelcted_vars){
    vn = paste0(base_name,gsub("q.","",v))
    vnq = paste0(base_name,v)
    sdqa[[vnq]] = recode(sdqa[[vn]], "0=2; 1=1; 2=0; else=NA")
  }
  
  rm(items,vn,vnq,v)

  sdqb = qu_b[,sdqbvars,with = F]
  
  items = c("EMOTIONX","CONCENTRATIONX","BEHAVIORX","SOCIALX")
  setnames(sdqb,paste(sdq_sav_stub,1:length(items),sep = "_"),paste0(base_name,items))

  imp_columns = setdiff(1:ncol(sdqb),c(1:2,grep("^SDQ",names(sdqb))))
  setnames(sdqb,
           names(sdqb)[imp_columns],
           paste0(base_name,"IPT.", sub("^p","",impact)))
  
  for (v in names(sdqa)[grep("IPT",names(sdqa))]){
    sdqb[[v]] = sdqb[[v]]-1
    attributes(sdqb[[v]]) = attributes(sdqa[[v]])
  }
  
  sdq = rbind(sdqa,sdqb,use.names = T, fill = T)
  
  for (vn in names(sdq)[grep("IPT",names(sdq))]) {
    vnq = sub("IPT.","IPT.q.",vn)
    sdq[[vnq]] = recode(sdq[[vn]], "0:1=0; 2=1; 3=2; NA=0")
  }
  
  for (d in setdiff(names(sdq_dims),"IMPACT")){
    df = sdq[,paste0(base_name,sdq_dims[[d]]),with = F]
    xn = apply(df, 1, function(x) sum(is.na(x)))
    sdq[[paste0(base_name,d,".SS")]] = 
      ifelse(xn<3, floor(0.5 + rowMeans(df, na.rm=TRUE)), NA)
  }
  rm(df,xn)
  
  if (rater == "P"){
    df = sdq[,paste0(base_name,"IPT.",sdq_dims[["IMPACT"]]),with = F]
    dfq = sdq[,paste0(base_name,"IPT.q.",sdq_dims[["IMPACT"]]),with = F]
    xn <- apply(df, 1, function(x) sum(is.na(x)))
    x <- ifelse(!xn==5, rowSums(dfq), NA)
    sdq[[paste0(base_name,"IPT.SS")]] <- as.numeric(ifelse(sdq$ebddiff==0, 0, x))
    
    sdq[[paste0(base_name,"TOT.SS")]] =
      rowSums(sdq[,paste0(base_name,setdiff(names(sdq_dims),c("IMPACT","PROSOC")),".SS"),with = F])
    attributes(sdq[[paste0(base_name,"TOT.SS")]]) = list(label = "sum of sumscores for emotion, conduct, hyper, peer")
  }  
  
  sdq = sdq[,-grep(paste0(base_name,"IPT.q"),names(sdq)),with = F]
  
  # note: names with capital letter are dimensions,
  #       names with small letter are items
  abbreviations = c(T = "teacher",
                    P = "parent",
                    IPT = "impact on functioning",
                    TOT = "global evaluation based on EMOTION, CONDUCT, HYPERACTIVITY, PEERRELATIONS",
                    SS = "sum of scores")
  sdq = add_label(sdq,"SDQ",abbreviations)
  return(sdq)
}
