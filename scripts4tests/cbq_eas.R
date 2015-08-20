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
  
  cbq = rbind(cbqa,cbqb,use.names = F)
  rm(cbqa,cbqb,cbqavars,cbqbvars)
  
  cbq_item_info = fread("instrument_docs/cbq_items_and_scale.txt")
  empathy_items = setnames(data.table(cbind(37:50,rep("Empathy",14))),c("V1","V2"),names(cbq_item_info))
  cbq_item_info = rbind(cbq_item_info,empathy_items)
  
  cbqitems = paste(paste("CBQ.parent.item",cbq_item_info$item_number,sep = ""),cbq_item_info$scale,sep = ".")
  setnames(cbq,paste("SBFCBQ",1:50,sep = ""),cbqitems )
  
  cbq_scales = unique(cbq_item_info$scale)
  for (s in cbq_scales) {
    cbq[[paste("CBQ.parent.sum.SCORE",s,sep = ".")]] = make_sum_scores(cbq[,grep(s,names(cbq)),with = F])
  }
  
  
  ############### need to add scale scores for EAS ####################
  # from mathiesen & tambs, 1999
  eas_scales = list(Emotionality = c(1,7,10),
                    Activity = c(2,4,-8),
                    Shyness = c(-5,6,-11),
                    Sociability = c(3,9,12))
  
  setnames(cbq,paste("SBFCBQ",51:62,sep = ""),paste("EAS.parent.item",1:12,sep = ""))
  for (s in names(eas_scales)) {
    w = eas_scales[[s]]
    cbq[[paste("EAS.parent.sum.SCORE",s,sep = ".")]] = 
      (as.matrix(cbq[,paste("EAS.parent.item",abs(w),sep = ""),with = F]) %*% sign(w))
  }
  cbq$EAS.parent.sum.SCORE.Shyness = cbq$EAS.parent.sum.SCORE.Shyness - min(cbq$EAS.parent.sum.SCORE.Shyness,na.rm = T)
  print("set minimum value of EAS.parent.sum.SCORE.Shyness to 0")
  return(cbq)
}