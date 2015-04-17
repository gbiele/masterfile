get_CRS = function(qu_a,qu_b){
  # Conners
  # A BHCONNERS_B1 - BHCONNERS_B25
  
  
  items2dimensions = list(ADHDA = c(1,4,12,14,16,17,22,23), # 14, 16 and 23 right here?
                          ADHDH = c(3,7,8,10,11,15,19,24,25),
                          ODD = c(2,6,9,18,5,20, 21)) # not sure about 20 and 21 here
  
  CRS_vars = names(qu_a)[1:2]
  for (d in names(items2dimensions)){
    old_names_a = paste("BHCONNERS_B",items2dimensions[[d]],sep = "")
    new_names = paste(paste("CRS.teacher.",d,".item",sep = ""),items2dimensions[[d]],sep = "")
    setnames(qu_a,old_names_a,new_names)
    CRS_vars = c(CRS_vars,new_names)
  }
  for (v in CRS_vars[-c(1,2)]) qu_b[[v]] = NA
  
  CRS = rbind(qu_a[,CRS_vars,with = F],qu_b[,CRS_vars,with = F])
}