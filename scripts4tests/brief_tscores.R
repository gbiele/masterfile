#library(XLConnect)
library(data.table)
library(plyr)
library(VIM)


add_briefTscores = function(MASTER) {
  table_parent = list(boy = readWorksheetFromFile(file = "item_info/BRIEF_tables.xlsx",
                                                  sheet = "P-Boys"),
                      girl = readWorksheetFromFile(file = "item_info/BRIEF_tables.xlsx",
                                                   sheet = "P-Girls"))
  
  brief_items = grep(paste0("BRF.P.*.i"),names(MASTER), value = T)
  brief_scores = data.frame(MASTER[,brief_items, with = F])
  few_missing_idx = which(rowSums(is.na(brief_scores)) < 30)
  brief_scores = brief_scores[few_missing_idx,]
  brief_scores_imputed = hotdeck(as.matrix(brief_scores))[,1:ncol(brief_scores)]
  
  brief_scores_imputed = brief_scores_imputed+1
  
  brief_scores_imputed = cbind(MASTER[few_missing_idx,c("PREG_ID_299", "BARN_NR","Gender"),with = F],
                               brief_scores_imputed)
  
  par(mfrow = c(5,2), mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01)
  
  my_scales = c("IH","SH","EC","WM","PO")
  for (s in my_scales) {
    
    item_names = grep(paste0("BRF.P.",s,".i"),names(MASTER), value = T)
    sum_scores = rowSums(brief_scores_imputed[,item_names,with = F])
    
    tscore_var = paste("BRF.P.",s,".TS")
    
    TS = rep(NA,nrow(brief_scores_imputed))
    for (g in c("boy","girl")) {
      from_scores = table_parent[[g]][,"Raw"]
      to_scores = table_parent[[g]][,paste0(s,".T")]
      from_scores = from_scores[!is.na(to_scores)]
      to_scores = to_scores[!is.na(to_scores)]
      
      idx = brief_scores_imputed$Gender == g
      TS[idx] = mapvalues(sum_scores[idx],
                          from = from_scores,
                          to = to_scores)
    }
    
    hist(sum_scores, main = paste(s,"SS"))
    hist(TS, main = paste(s,"T"))
    
    TS = cbind(brief_scores_imputed[,c("PREG_ID_299", "BARN_NR"), with = F],
               TS)
    setnames(TS,"TS",paste0("BRF.P.",s,".TS"))
    
    MASTER = merge(MASTER,TS,
                   by = c("PREG_ID_299", "BARN_NR"),
                   all.x = T)
    
    return(MASTER)
  } 
}