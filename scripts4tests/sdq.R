get_sdq = function(pqa,pqb){

  
  ################# SDQ #######################
  # scoring: http://www.sdqinfo.org/c9.html
  
  sdqavars = c("PREG_ID_299","BARN_NR",names(pqa)[grep("SBFSDQ",names(pqa))])
  sdqavars = setdiff(sdqavars,c("SBFSDQ26","SBFSDQ28","SBFSDQ34"))
  sdqa = pqa[,sdqavars,with = F]
  
  items = c("consid", "restles","somatic","shares","tantrum","loner",
            "obeys","worries","caring","fidgety","friend","fights",
            "unhappy","popular","distrac","clingy","kind","lies",
            "bullied","helpout","reflect","steals","oldbest","afraid",
            "attends")
  for (i in 1:length(items)) setnames(sdqa,paste("SBFSDQ",i,sep = ""),paste("SDQ.parent.item",items[i],sep = ".p"))
  
  setnames(sdqa,"SBFSDQ27","pebddiff")
  impact = c("distres","imphome","impfrie","impclas","impleis")
  for (i in 1:length(impact)) {
    setnames(sdqa,paste("SBFSDQ",i+28,sep = ""),paste("SDQ.parent.item",impact[i],sep = ".p"))
    vn = paste("SDQ.parent.item",impact[i],sep = ".p")
    vnq = paste("SDQ.parent.item",impact[i],sep = ".q")
    x = as.numeric((sdqa[[vn]]-1) * (sdqa[[vn]] > 1))
    sdqa = eval(parse(text = paste("sdqa[,",vnq,":= x]")))
  }
  
  
  for (v in c("obeys","reflect","attends","friend","popular")){
    vn = paste("SDQ.parent.item",v,sep = ".p")
    vnq = paste("SDQ.parent.item",v,sep = ".q")
    sdqa = eval(parse(text = paste("sdqa[,",vnq,":= abs(",vn,"-2)]")))
  }
  
  
  sdq_dims = list(emotion = c("psomatic", "pworries", "punhappy", "pclingy", "pafraid"),
                  conduct = c("ptantrum", "qobeys", "pfights", "plies", "psteals"),
                  hyper  = c("prestles", "pfidgety", "pdistrac", "qreflect", "qattends"),
                  peer = c("ploner", "qfriend", "qpopular", "pbullied", "poldbest"),
                  prosoc  = c("pconsid", "pshares", "pcaring", "pkind", "phelpout"),
                  impact  = c("pdistres", "pimphome", "pimpfrie", "pimpclas", "pimpleis"))
  
  
  rm(i,impact,items,sdqavars,vn,vnq,v,x)
  
  
  sdqbvars = c("PREG_ID_299","BARN_NR",names(pqb)[grep("C_31",names(pqb))])
  sdqbvars = setdiff(sdqbvars,c("C_31_5","C_3111","C_3112"))
  sdqb = pqb[,sdqbvars,with = F]
  
  items = c("Emotions","Concentration","Behavior","Social")
  for (i in 1:length(items)) setnames(sdqb,paste("C_31_",i,sep = ""),paste("SDQ.parent.item",items[i],sep = "."))
  impact = c("pdistres", "pimphome", "pimpfrie", "pimpclas", "pimpleis")
  
  for (i in 1:length(sdq_dims[["impact"]])) {
    
    vn = paste("SDQ.parent.item", sdq_dims[["impact"]][i],sep = ".")
    vnq = sub("SDQ.parent.item.p","SDQ.parent.item.q",vn)
    
    if ((i+5)<10) {setnames(sdqb,paste("C_31_",i+5,sep = ""),vn)
    } else {setnames(sdqb,paste("C_31",i+5,sep = ""),vn)}
    
    x = as.numeric((sdqb[[vn]]-1) * (sdqb[[vn]] > 1))
    sdqb = eval(parse(text = paste("sdqb[,",vnq,":= x]")))
  }
  
  
  sdq = rbind(sdqa,sdqb,use.names = T, fill = T)
  
  for (d in names(sdq_dims)) {
    dimdat = data.frame(sdq[,paste("SDQ.parent.item",sdq_dims[[d]],sep = "."),with = F])
    tmp <- apply(dimdat, 1, function(x) sum(is.na(x)))
    tmp <- ifelse(tmp<3, rowMeans(dimdat, na.rm=TRUE), NA)
    tmp <- as.numeric(tmp) * 5
    tmp <- floor(0.5 + tmp)
    eval(parse(text = paste("sdq$SDQ.parent.score.",d, "= tmp",sep = "")))
  }
  
  return(sdq)
}
