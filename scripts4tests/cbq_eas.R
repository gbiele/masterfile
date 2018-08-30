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

# info short form: https://research.bowdoin.edu/rothbart-temperament-questionnaires/files/2016/09/cbq_short_form_paper.pdf 

get_cbq_eas = function(pqa,pqb){
  cbqavars = c(index_vars,names(pqa)[grep("SBFCBQ",names(pqa))])
  cbqa = pqa[,cbqavars,with = F]
  
  cbqbvars = c(index_vars,names(pqb)[grep("C_28",names(pqb))])
  cbqb = pqb[,cbqbvars,with = F]
  
  for (v in 3:ncol(cbqa)){
    attributes(cbqb[[v]]) = attributes(cbqa[[v]])
    cbqa[[v]][c(which(cbqa[[v]] > 7),which(cbqa[[v]] == 0))] = NA
    cbqb[[v]][c(which(cbqb[[v]] > 7),which(cbqb[[v]] == 0))] = NA
  }
  
  cbq = rbind(cbqa,cbqb,use.names = F)
  rm(cbqa,cbqb,cbqavars,cbqbvars)
  
  cbq_item_info = fread("instrument_docs/cbq_items_and_scale.txt")
  cbq_item_info[,item_direction := sign(item_number)]
  cbq_item_info[,item_number := abs(item_number)]
  setkey(cbq_item_info,"item_number")
  empathy_items = data.table(item_number = 37:50, scale = "Empathy", item_direction = 1)
  empathy_items[item_number %in% c(44,45,48,49), item_direction := -1]
  cbq_item_info = rbind(cbq_item_info,empathy_items)
  
  cbqitems = paste(paste0("CBQ.P.",cbq_item_info$scale),
                   cbq_item_info$item_number,
                   sep = ".i")
  
  setnames(cbq,paste("SBFCBQ",1:50,sep = ""),cbqitems )
  
  
  ## reflect some items
  ritems = paste(paste0("CBQ.P.",
                       cbq_item_info[item_direction < 0,scale]),
                cbq_item_info[item_direction < 0,item_number],
                sep = ".i")
  for ( i in ritems) {
    cbq[[i]] = abs(cbq[[i]]-8)
  }
  
  
  ############### items for EAS ####################
  # from mathiesen & tambs, 1999
  eas_scales = list(EMOTION = c(1,7,10),
                    ACTIVITY = c(2,4,-8),
                    SHYNESS = c(-5,6,-11),
                    SOCIABIL = c(3,9,12))
  scl = rep("n",length(unlist(eas_scales)))
  for (s in names(eas_scales)) scl[abs(eas_scales[[s]])] = s
  
  setnames(cbq,paste("SBFCBQ",51:62,sep = ""),paste0("EAS.P.",scl,".i",1:12))
  
  cbq = smart_impute(cbq)
  
  ########### sum scores #############
  
  cbq_scales = unique(cbq_item_info$scale)
  iteminfo = ""
  for (s in cbq_scales) {
    iteminfo = paste(iteminfo,
                     paste0("#### ",s, " ####"),
                     "\n",
                     paste(sapply(cbq[,grep(s,names(cbq)),with = F], attr, "label"),collapse = "\n"),
                     "\n")
    cbq = make_sum_scores(cbq,grep(s,names(cbq)),paste0("CBQ.P.",s,".SS"))
  }
  cat(iteminfo,file = "item_info/cbq.txt")
  
  for (d in names(eas_scales)) {
    for (item in abs(eas_scales[[d]][eas_scales[[d]]<0])) {
      item_name = paste0("EAS.P.",d,".i",item)
      cbq[[item_name]] = cbq[[item_name]]*-1+max(cbq[[item_name]],na.rm = T)+1
    }
  }
  
  iteminfo = ""
  for (s in names(eas_scales)) {
    iteminfo = paste(iteminfo,
                     paste0("#### ",s, " ####"),
                     "\n",
                     paste(sapply(cbq[,grep(s,names(cbq)),with = F], attr, "label"),collapse = "\n"),
                     "\n")
    cbq = make_sum_scores(cbq,grep(s,names(cbq)),paste0("EAS.P.",s,sep = ".SS"))
  }
  cat(iteminfo,file = "item_info/eas.txt")
  
  abbreviations = c(CBQ = "CBQ",
                    EAS = "EAS",
                    P = "Parent rating",
                    SS = "sum score",
                    SC = "count of non-zero scores",
                    EMOTION = "EMOTIONABILITY",
                    SOCIABIL = "SOCIABILITY")
  cbq = add_label(cbq,"CBQ",abbreviations)
  cbq = add_label(cbq,"EAS",abbreviations)
  
  cbq_labels = 1:7
  names(cbq_labels) = c("extremely untrue of your child",
                        "quite untrue of your child",
                        "slightly untrue of your child",
                        "neither true nor false of your child",
                        "slightly true of your child",
                        "quite true of your child",
                        "extremely true of your child")
  for (v in names(cbq)[grep("\\.i[0-9]",names(cbq))])
    attributes(cbq[[v]])$labels = cbq_labels
  
  return(cbq)
}

