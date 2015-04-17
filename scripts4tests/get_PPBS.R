get_PPBS = function(qu_a,qu_b){
  # COPLAND
  # A BHCOPLAND1 - BHCOPLAND21
  # B B__1_1 - B__1_21
  
  # 1. Talks to other children during play
  # 2. Plays by himself/herself, examining a toy or object
  # 3. Plays "rough-and-tumble" with other children
  # 4. Takes on the role of onlooker or spectator
  # 5. Plays "make-believe" with other children
  # 6. Engages in group play
  # 7. Engages in pretend play by himself/herself
  # 8. Plays alone, building things with blocks and/or other toys
  # 9. Wanders around aimlessly
  # 10. Plays in groups with (not just besides) other children
  # 11. Plays "make-believe", but not with other children
  # 12. Watches or listens to other children without trying to join in
  # 13. Plays "rough-and-tumble" with other children
  # 14. Plays by himself/herself, drawing, painting pictures, or doing puzzles
  # 15. Talks to other children during play
  # 16. Engages in pretend play with other children
  # 17. Plays alone, exploring toys and objects, trying to figure out how they work
  # 18. Remains alone and unoccupied, perhaps staring off into space
  # 19. Finds it important to keep his/her toys in order
  # 20. Wants to lead and decide what others can do during play
  # 21. Plays only a short while with each toy, does not stick to anything
  
  items2dimensions = list(Reticent = c(4,9,12,18),
                          SolitaryPassive = c(2,14,17),
                          SolitaryActive = c(7,8,11),
                          SocialPlay = c(1,5,6,10,15,16),
                          RoughPlay = c(3,13))
  
  PPBS_vars = names(qu_a)[1:2]
  for (d in names(items2dimensions)){
    old_names_a = paste("BHCOPLAND",items2dimensions[[d]],sep = "")
    old_names_b = paste("B__1",sub(" ","_",sprintf("%2i",items2dimensions[[d]])),sep = "")
    new_names = paste(paste("PPBS.teacher.",d,".item",sep = ""),items2dimensions[[d]],sep = "")
    setnames(qu_a,old_names_a,new_names)
    setnames(qu_b,old_names_b,new_names)
    PPBS_vars = c(PPBS_vars,new_names)
  }
  
  PPBS = rbind(qu_a[,PPBS_vars,with = F],qu_b[,PPBS_vars,with = F])
}
