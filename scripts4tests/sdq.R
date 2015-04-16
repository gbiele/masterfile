get_sdq = function(qu_a,qu_b,rater){
  
  if (rater == "parent") {
    sdqavars = c("PREG_ID_299","BARN_NR",names(qu_a)[grep("SBFSDQ",names(qu_a))])
    sdqavars = setdiff(sdqavars,c("SBFSDQ26","SBFSDQ28","SBFSDQ34"))
    sdqbvars = c("PREG_ID_299","BARN_NR",names(qu_b)[grep("C_31",names(qu_b))])
    sdqbvars = setdiff(sdqbvars,c("C_31_5","C_3111","C_3112"))
    base_name = "SDQ.parent." 
    sbase = "SBFSDQ"
    impact = c("distres","imphome","impfrie","impclas","impleis")
    sdq_sav_stub = "C_31"
  } else if (rater == "teacher") {
    sdqavars = c("PREG_ID_299","BARN_NR",names(qu_a)[grep("BHSDQ",names(qu_a))])
    sdqavars = setdiff(sdqavars,c("BHSDQ26","BHSDQ28","BHSDQ34"))
    sdqbvars = c("PREG_ID_299","BARN_NR",names(qu_b)[grep("B__5",names(qu_b))])
    sdqbvars = setdiff(sdqbvars,c("B__5_5","B__511","B__512"))
    base_name = "SDQ.teacher." 
    sbase = "BHSDQ_B"
    impact = c("distres","impfrie","impclas")
    sdq_sav_stub = "B__5"
  }
  
  ################# SDQ #######################
  # scoring: http://www.sdqinfo.org/c9.html
  

  sdqa = qu_a[,sdqavars,with = F]
  
  items = c("consid", "restles","somatic","shares","tantrum","loner",
            "obeys","worries","caring","fidgety","friend","fights",
            "unhappy","popular","distrac","clingy","kind","lies",
            "bullied","helpout","reflect","steals","oldbest","afraid",
            "attends")
  for (i in 1:length(items)) setnames(sdqa,paste(sbase,i,sep = ""), paste(base_name,items[i],sep = "item.p"))
  
  setnames(sdqa,paste(sbase,27,sep = ""),"pebddiff")
  for (i in 1:length(impact)) {
    setnames(sdqa,paste(sbase,i+28,sep = ""),paste(base_name,impact[i],sep = "item.p"))
    vn = paste(base_name,impact[i],sep = "item.p")
    vnq = paste(base_name,impact[i],sep = "item.q")
    x = as.numeric((sdqa[[vn]]-1) * (sdqa[[vn]] > 1))
    sdqa = eval(parse(text = paste("sdqa[,",vnq,":= x]")))
  }
  
  
  for (v in c("obeys","reflect","attends","friend","popular")){
    vn = paste(base_name,v,sep = "item.p")
    vnq = paste(base_name,v,sep = "item.q")
    sdqa = eval(parse(text = paste("sdqa[,",vnq,":= abs(",vn,"-2)]")))
  }
  
  
  sdq_dims = list(emotion = c("psomatic", "pworries", "punhappy", "pclingy", "pafraid"),
                  conduct = c("ptantrum", "qobeys", "pfights", "plies", "psteals"),
                  hyper  = c("prestles", "pfidgety", "pdistrac", "qreflect", "qattends"),
                  peer = c("ploner", "qfriend", "qpopular", "pbullied", "poldbest"),
                  prosoc  = c("pconsid", "pshares", "pcaring", "pkind", "phelpout"),
                  impact  = paste("p",impact,sep = ""))
  
  
  rm(i,items,sdqavars,vn,vnq,v,x)
  
  impact = paste("p",impact,sep = "")  

  sdqb = qu_b[,sdqbvars,with = F]
  
  items = c("Emotions","Concentration","Behavior","Social")
  for (i in 1:length(items)) setnames(sdqb,paste(sdq_sav_stub,i,sep = "_"),paste(base_name,items[i],sep = "item."))
  
  
  for (i in 1:length(sdq_dims[["impact"]])) {
    
    vn = paste(base_name, sdq_dims[["impact"]][i],sep = "item.")
    vnq = sub(paste(base_name,"item.p",sep = ""),paste(base_name,"item.q",sep = ""),vn)
    
    if ((i+5)<10) {setnames(sdqb,paste(sdq_sav_stub,i+5,sep = "_"),vn)
    } else {setnames(sdqb,paste(sdq_sav_stub,i+5,sep = ""),vn)}
    
    x = as.numeric((sdqb[[vn]]-1) * (sdqb[[vn]] > 1))
    sdqb = eval(parse(text = paste("sdqb[,",vnq,":= x]")))
  }
  
  
  sdq = rbind(sdqa,sdqb,use.names = T, fill = T)
  
  for (d in names(sdq_dims)) {
    dimdat = data.frame(sdq[,paste(base_name,sdq_dims[[d]],sep = "item."),with = F])
    tmp <- apply(dimdat, 1, function(x) sum(is.na(x)))
    tmp <- ifelse(tmp<3, rowMeans(dimdat, na.rm=TRUE), NA)
    tmp <- as.numeric(tmp) * 5
    tmp <- floor(0.5 + tmp)
    eval(parse(text = paste("sdq$",base_name,"score.",d, "= tmp",sep = "")))
  }
  
  return(sdq)
}
