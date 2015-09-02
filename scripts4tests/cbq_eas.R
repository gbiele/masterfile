# from the instrument documentation:
# CBQ: We have used the 36 questions from CBQ-VSF (Very Short Form). These are
# translated by Anne-Mari Torgersen. In addition we have the empathy questions from the
# CBQ Additional Measures (14 questions, number 37-50 in our questionnaire), which are
# translated by the ABC/ADHD study.
# In the 2009 version of the questionnaire, we have changed the last response category
# "Passer Ikke" (Does not fit) to "Ikke Aktuelt" (Does Not Apply), with Torgersen's approval.
# EAS: We have used the same 12 questions from EAS that MoBa have in their Q6
# questionnaire. We have also included the two additional questions that are at the end of EAS
# in Q6 (made by MoBa).

# info short form: http://www.bowdoin.edu/~sputnam/rothbart-temperament-questionnaires/pdf/CBQ-development-of-short-form.pdf 

get_cbq_eas = function(pqa,pqb){
  cbqavars = c("PREG_ID_299","BARN_NR",names(pqa)[grep("SBFCBQ",names(pqa))])
  cbqa = pqa[,cbqavars,with = F]
  
  cbqbvars = c("PREG_ID_299","BARN_NR",names(pqb)[grep("C_28",names(pqb))])
  cbqb = pqb[,cbqbvars,with = F]
  
  for (v in 3:ncol(cbqa)){
    attributes(cbqb[[v]]) = attributes(cbqa[[v]])
    cbqa[[v]][which(cbqa[[v]] == 9)] = NA
  }
  
  cbq = rbind(cbqa,cbqb,use.names = F)
  
  
  
  rm(cbqa,cbqb,cbqavars,cbqbvars)
  
  cbq_item_info = fread("instrument_docs/cbq_items_and_scale.txt")
  empathy_items = setnames(data.table(cbind(37:50,rep("Empathy",14))),c("V1","V2"),names(cbq_item_info))
  cbq_item_info = rbind(cbq_item_info,empathy_items)
  
  cbqitems = paste(paste("CBQ.P.PI",cbq_item_info$item_number,sep = ""),cbq_item_info$scale,sep = ".")
  setnames(cbq,paste("SBFCBQ",1:50,sep = ""),cbqitems )
  
  cbq_scales = unique(cbq_item_info$scale)
  for (s in cbq_scales) {
    cbq = make_sum_scores(cbq,grep(s,names(cbq)),paste("CBQ.P.SS",s,sep = "."))
  }
  
  
  ############### need to add scale scores for EAS ####################
  # from mathiesen & tambs, 1999
  eas_scales = list(EMOTIONALITY = c(1,7,10),
                    ACTIVITY = c(2,4,-8),
                    SHYNESS = c(-5,6,-11),
                    SOCIABILITY = c(3,9,12))
  
  setnames(cbq,paste("SBFCBQ",51:62,sep = ""),paste0("EAS.P.I.",1:12))
  for (s in names(eas_scales)) {
    w = eas_scales[[s]]
    cbq[[paste("EAS.P.SS",s,sep = ".")]] = 
      (as.matrix(cbq[,paste0("EAS.P.I.",abs(w)),with = F]) %*% sign(w))
  }
  cbq$EAS.P.SS.SHYNESS = cbq$EAS.P.SS.SHYNESS - min(cbq$EAS.P.SS.SHYNESS,na.rm = T)
  print("set minimum value of EAS.P.SS.SHYNESS to 0")
  return(cbq)
}