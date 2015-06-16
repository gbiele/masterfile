#
# Recoding SPSS syntax for Stanford Binet
# March 2015, Ida H. Caspersen
#*********************************************
#
# In this script, scaled scores are calculated for 
## - Non-verbal routing (NVIQ)
## - Verbal routing (VIQ)
## - Non-verbal working memory (NVWMS)
## - Verbal working memory (VWMS) 
## - Fluid reasoning (fortelle til bilder/verbal oppgave)
# Also calculating an abbreviated IQ score

# Required data files:
# STB_V1_V2_V3.sav
# Screening1_2.sav (?) --> Age_mo variable is here


StBdata <- read_sav("savs/StB.sav");

##Ida## Generating "Age_mo" from ADHD_SCORE_NY.sav
names(StBdata)[names(StBdata)=="Barn_nr"] <- "BARN_NR"
ADHDscoreNY_vars <- ADHDscoreNY[,c("PREG_ID_299","BARN_NR","Age")]
StBdata <- merge(StBdata, ADHDscoreNY_vars, by = c("PREG_ID_299","BARN_NR"),all.x = T)
StBdata[,"Age_mo"] <- StBdata[,"Age"]/365*12


################################ Non-verbal routing ################################
NVIQ <-
  ifelse(StBdata[,"Age_mo"]>=38 & StBdata[,"Age_mo"]<42  & StBdata[,"SB1_2"] %in% c(0:16), 
         StBdata[,"SB1_2"] + 3,
    ifelse(StBdata[,"Age_mo"]>=38 & StBdata[,"Age_mo"]<42& StBdata[,"SB1_2"] >16, 19,
      ifelse(StBdata[,"Age_mo"]>=42 & StBdata[,"Age_mo"]<48 & StBdata[,"SB1_2"] %in% c(0:5,8:16), 
          StBdata[,"SB1_2"] + 2, 
        ifelse(StBdata[,"Age_mo"]>=42 & StBdata[,"Age_mo"]<48 & StBdata[,"SB1_2"] %in% c(6,7),
           StBdata[,"SB1_2"] + 3,
          ifelse(StBdata[,"Age_mo"]>=42 & StBdata[,"Age_mo"]<48 & StBdata[,"SB1_2"]>16, 19, NA)))))


################################ Verbal routing ################################
# First, creating new corrected variable for total score for verbal routing.
# NB: In SPSS, NAs are treated as 0 when added to a value.
# Recoding to resemble the variable generated in SPSS (if desireable...):
StBdata[is.na(StBdata[,"SB3_2"]),"SB3_2"] <- 0
StBdata[is.na(StBdata[,"SB3_3"]),"SB3_3"] <- 0
StBdata[,"SB3_4_Ny"]<- StBdata[,"SB3_2"]+ StBdata[,"SB3_3"]
VIQ <- 
  ifelse(StBdata[,"Age_mo"]>=38 & StBdata[,"Age_mo"]<40,
   ifelse(StBdata[,"SB3_4_Ny"] %in% 0:1, 1,
    ifelse(StBdata[,"SB3_4_Ny"] %in% 2:3, StBdata[,"SB3_4_Ny"]-1,
     ifelse(StBdata[,"SB3_4_Ny"] %in% 4:7, StBdata[,"SB3_4_Ny"]-2, 
      ifelse(StBdata[,"SB3_4_Ny"] %in% 8:11, StBdata[,"SB3_4_Ny"]-3,
       ifelse(StBdata[,"SB3_4_Ny"] %in% 12:15, StBdata[,"SB3_4_Ny"]-4,
        ifelse(StBdata[,"SB3_4_Ny"] %in% 16:19, StBdata[,"SB3_4_Ny"]-5,
         ifelse(StBdata[,"SB3_4_Ny"] %in% 20:23, StBdata[,"SB3_4_Ny"]-6,
          ifelse(StBdata[,"SB3_4_Ny"] %in% 24:26, StBdata[,"SB3_4_Ny"]-7,
           ifelse(StBdata[,"SB3_4_Ny"]>26, 19, NA))))))))),
    ifelse(StBdata[,"Age_mo"]>=40 & StBdata[,"Age_mo"]<42,
     ifelse(StBdata[,"SB3_4_Ny"] %in% 0:1, 1,
      ifelse(StBdata[,"SB3_4_Ny"] %in% 2:3, StBdata[,"SB3_4_Ny"]-1,
       ifelse(StBdata[,"SB3_4_Ny"] %in% 4:7, StBdata[,"SB3_4_Ny"]-2, 
        ifelse(StBdata[,"SB3_4_Ny"] %in% 8:10, StBdata[,"SB3_4_Ny"]-3,
         ifelse(StBdata[,"SB3_4_Ny"] %in% 11:14, StBdata[,"SB3_4_Ny"]-4,
          ifelse(StBdata[,"SB3_4_Ny"] %in% 15:17, StBdata[,"SB3_4_Ny"]-5,
           ifelse(StBdata[,"SB3_4_Ny"] %in% 18:21, StBdata[,"SB3_4_Ny"]-6,
            ifelse(StBdata[,"SB3_4_Ny"] %in% 22:24, StBdata[,"SB3_4_Ny"]-7,
             ifelse(StBdata[,"SB3_4_Ny"] %in% 25:27, StBdata[,"SB3_4_Ny"]-8,
              ifelse(StBdata[,"SB3_4_Ny"] >27, 19, NA)))))))))),
      ifelse(StBdata[,"Age_mo"]>=42 & StBdata[,"Age_mo"]<44,
       ifelse(StBdata[,"SB3_4_Ny"] %in% 0:3, 1,
        ifelse(StBdata[,"SB3_4_Ny"]==4, 2,
         ifelse(StBdata[,"SB3_4_Ny"] %in% 5:7, StBdata[,"SB3_4_Ny"]-3, 
          ifelse(StBdata[,"SB3_4_Ny"] %in% 8:11, StBdata[,"SB3_4_Ny"]-4,
           ifelse(StBdata[,"SB3_4_Ny"] %in% 12:15, StBdata[,"SB3_4_Ny"]-5,
            ifelse(StBdata[,"SB3_4_Ny"] %in% 16:19, StBdata[,"SB3_4_Ny"]-6,
             ifelse(StBdata[,"SB3_4_Ny"] %in% 20:23, StBdata[,"SB3_4_Ny"]-7,
              ifelse(StBdata[,"SB3_4_Ny"] %in% 24:26, StBdata[,"SB3_4_Ny"]-8,
               ifelse(StBdata[,"SB3_4_Ny"] %in% 27:28, StBdata[,"SB3_4_Ny"]-9,
                ifelse(StBdata[,"SB3_4_Ny"] >28, 19, NA)))))))))),
       ifelse(StBdata[,"Age_mo"]>=44 & StBdata[,"Age_mo"]<46,
        ifelse(StBdata[,"SB3_4_Ny"] %in% 0:4, 1,
          ifelse(StBdata[,"SB3_4_Ny"] %in% 5:7, StBdata[,"SB3_4_Ny"]-3, 
           ifelse(StBdata[,"SB3_4_Ny"] %in% 8:10, StBdata[,"SB3_4_Ny"]-4,
            ifelse(StBdata[,"SB3_4_Ny"] %in% 11:14, StBdata[,"SB3_4_Ny"]-5,
             ifelse(StBdata[,"SB3_4_Ny"] %in% 15:17, StBdata[,"SB3_4_Ny"]-6,
              ifelse(StBdata[,"SB3_4_Ny"] %in% 18:21, StBdata[,"SB3_4_Ny"]-7,
               ifelse(StBdata[,"SB3_4_Ny"] %in% 22:24, StBdata[,"SB3_4_Ny"]-8,
                ifelse(StBdata[,"SB3_4_Ny"] %in% 25:27, StBdata[,"SB3_4_Ny"]-9,
                 ifelse(StBdata[,"SB3_4_Ny"] %in% 28:29, StBdata[,"SB3_4_Ny"]-10,
                  ifelse(StBdata[,"SB3_4_Ny"] >29, 19, NA)))))))))),
          ifelse(StBdata[,"Age_mo"]>=46 & StBdata[,"Age_mo"]<48,
           ifelse(StBdata[,"SB3_4_Ny"] %in% 0:4, 1,
            ifelse(StBdata[,"SB3_4_Ny"]==5, 2,
             ifelse(StBdata[,"SB3_4_Ny"] %in% 6:8, StBdata[,"SB3_4_Ny"]-4, 
              ifelse(StBdata[,"SB3_4_Ny"] %in% 9:12, StBdata[,"SB3_4_Ny"]-5,
               ifelse(StBdata[,"SB3_4_Ny"] %in% 13:15, StBdata[,"SB3_4_Ny"]-6,
                ifelse(StBdata[,"SB3_4_Ny"] %in% 16:18, StBdata[,"SB3_4_Ny"]-7,
                 ifelse(StBdata[,"SB3_4_Ny"] %in% 19:22, StBdata[,"SB3_4_Ny"]-8,
                  ifelse(StBdata[,"SB3_4_Ny"] %in% 23:25, StBdata[,"SB3_4_Ny"]-9,
                   ifelse(StBdata[,"SB3_4_Ny"] %in% 26:28, StBdata[,"SB3_4_Ny"]-10,
                    ifelse(StBdata[,"SB3_4_Ny"] %in% 29:30, StBdata[,"SB3_4_Ny"]-11,
                     ifelse(StBdata[,"SB3_4_Ny"] >30, 19, NA))))))))))),NA)))))


################################ Non-verbal working memory ################################
NVWMS <- 
  ifelse(StBdata[,"Age_mo"]>=38 & StBdata[,"Age_mo"]<40,
     ifelse(StBdata[,"SB5_5"] %in% 0:1, StBdata[,"SB5_5"]+2,
        ifelse(StBdata[,"SB5_5"] %in% 2:4, StBdata[,"SB5_5"]+3,
           ifelse(StBdata[,"SB5_5"] %in% 5:15, StBdata[,"SB5_5"]+4,
              ifelse(StBdata[,"SB5_5"] >15, 19, NA)))),
    ifelse(StBdata[,"Age_mo"]>=40 & StBdata[,"Age_mo"]<42,
       ifelse(StBdata[,"SB5_5"] %in% 0:1, StBdata[,"SB5_5"]+2,
          ifelse(StBdata[,"SB5_5"] %in% 2:10, StBdata[,"SB5_5"]+3,
             ifelse(StBdata[,"SB5_5"] %in% 11:15, StBdata[,"SB5_5"]+4,
                ifelse(StBdata[,"SB5_5"] >15, 19, NA)))),
      ifelse(StBdata[,"Age_mo"]>=42 & StBdata[,"Age_mo"]<44,
         ifelse(StBdata[,"SB5_5"] %in% 0:1, StBdata[,"SB5_5"]+1,
            ifelse(StBdata[,"SB5_5"] %in% 2:4, StBdata[,"SB5_5"]+2,
               ifelse(StBdata[,"SB5_5"] %in% 5:16, StBdata[,"SB5_5"]+3,
                  ifelse(StBdata[,"SB5_5"] >16, 19, NA)))),              
        ifelse(StBdata[,"Age_mo"]>=44 & StBdata[,"Age_mo"]<46,
           ifelse(StBdata[,"SB5_5"] %in% 0:1, StBdata[,"SB5_5"]+1,
              ifelse(StBdata[,"SB5_5"] %in% 2:10, StBdata[,"SB5_5"]+2,
                 ifelse(StBdata[,"SB5_5"] %in% 11:16, StBdata[,"SB5_5"]+3,
                    ifelse(StBdata[,"SB5_5"] >16, 19, NA)))), 
          ifelse(StBdata[,"Age_mo"]>=46 & StBdata[,"Age_mo"]<48,
             ifelse(StBdata[,"SB5_5"] %in% 0:3, StBdata[,"SB5_5"]+1,
                ifelse(StBdata[,"SB5_5"] %in% 4:17, StBdata[,"SB5_5"]+2,
                  ifelse(StBdata[,"SB5_5"] >17, 19, NA))), NA)))))
              

################################ Verbal working memory ################################
VWMS <- 
  ifelse(StBdata[,"Age_mo"]>=38 & StBdata[,"Age_mo"]<40,
     ifelse(StBdata[,"SB7_5"] %in% 0:3, StBdata[,"SB7_5"]+3,
        ifelse(StBdata[,"SB7_5"] %in% 4:15, StBdata[,"SB7_5"]+4,
              ifelse(StBdata[,"SB7_5"] >15, 19, NA))),
    ifelse(StBdata[,"Age_mo"]>=40 & StBdata[,"Age_mo"]<42,
       ifelse(StBdata[,"SB7_5"] %in% 0:12, StBdata[,"SB7_5"]+3,
          ifelse(StBdata[,"SB7_5"] %in% 13:15, StBdata[,"SB7_5"]+4,
             ifelse(StBdata[,"SB7_5"] >15, 19, NA))),        
      ifelse(StBdata[,"Age_mo"]>=42 & StBdata[,"Age_mo"]<44,
         ifelse(StBdata[,"SB7_5"] %in% 0:3, StBdata[,"SB7_5"]+2,
            ifelse(StBdata[,"SB7_5"] %in% 4:16, StBdata[,"SB7_5"]+3,
               ifelse(StBdata[,"SB7_5"] >16, 19, NA))),
        ifelse(StBdata[,"Age_mo"]>=44 & StBdata[,"Age_mo"]<46,
           ifelse(StBdata[,"SB7_5"] %in% 0:1, StBdata[,"SB7_5"]+1,
              ifelse(StBdata[,"SB7_5"] %in% 2:10, StBdata[,"SB7_5"]+2,
                 ifelse(StBdata[,"SB7_5"] %in% 11:16, StBdata[,"SB7_5"]+3,
                    ifelse(StBdata[,"SB7_5"] >16, 19, NA)))),   
           ifelse(StBdata[,"Age_mo"]>=46 & StBdata[,"Age_mo"]<48,
              ifelse(StBdata[,"SB7_5"] %in% 0:17, StBdata[,"SB7_5"]+2,
                    ifelse(StBdata[,"SB7_5"] >17, 19, NA)), NA)))))         
           

################################ Fluid reasoning ################################         
VOS <- 
  ifelse(StBdata[,"Age_mo"]>=38 & StBdata[,"Age_mo"]<40,
   ifelse(StBdata[,"SB6_5"]==0, 3,
    ifelse(StBdata[,"SB6_5"]==1, 5,
     ifelse(StBdata[,"SB6_5"]==2, 7,             
      ifelse(StBdata[,"SB6_5"] %in% 3:4, StBdata[,"SB6_5"]+6,
       ifelse(StBdata[,"SB6_5"] %in% 5:7, StBdata[,"SB6_5"]+7,  
        ifelse(StBdata[,"SB6_5"] %in% 8:11, StBdata[,"SB6_5"]+8,
         ifelse(StBdata[,"SB6_5"] >11, 19, NA))))))),      
     ifelse(StBdata[,"Age_mo"]>=40 & StBdata[,"Age_mo"]<42,
      ifelse(StBdata[,"SB6_5"]==0, 3,
       ifelse(StBdata[,"SB6_5"]==1, 5,
        ifelse(StBdata[,"SB6_5"]==2, 7,             
         ifelse(StBdata[,"SB6_5"] %in% 3:5, StBdata[,"SB6_5"]+6,
          ifelse(StBdata[,"SB6_5"] %in% 6:12, StBdata[,"SB6_5"]+7,  
           ifelse(StBdata[,"SB6_5"] >12, 19, NA)))))),   
       ifelse(StBdata[,"Age_mo"]>=42 & StBdata[,"Age_mo"]<44,
         ifelse(StBdata[,"SB6_5"]==0, 3,
          ifelse(StBdata[,"SB6_5"]==1, 5,             
           ifelse(StBdata[,"SB6_5"] %in% 2:4, StBdata[,"SB6_5"]+5,
            ifelse(StBdata[,"SB6_5"] %in% 5:7, StBdata[,"SB6_5"]+6, 
             ifelse(StBdata[,"SB6_5"] %in% 8:11, StBdata[,"SB6_5"]+7,
              ifelse(StBdata[,"SB6_5"] %in% 12:13, StBdata[,"SB6_5"]+6,  
               ifelse(StBdata[,"SB6_5"] >13, 19, NA))))))),    
          ifelse(StBdata[,"Age_mo"]>=44 & StBdata[,"Age_mo"]<46,
           ifelse(StBdata[,"SB6_5"]==0, 3,
            ifelse(StBdata[,"SB6_5"]==1, 5,             
             ifelse(StBdata[,"SB6_5"] %in% 2:5, StBdata[,"SB6_5"]+5,
              ifelse(StBdata[,"SB6_5"] %in% 6:13, StBdata[,"SB6_5"]+6, 
               ifelse(StBdata[,"SB6_5"] >13, 19, NA))))),  
            ifelse(StBdata[,"Age_mo"]>=46 & StBdata[,"Age_mo"]<48,
             ifelse(StBdata[,"SB6_5"]==0, 3,
              ifelse(StBdata[,"SB6_5"]==1, 5,             
               ifelse(StBdata[,"SB6_5"] %in% 2:10, StBdata[,"SB6_5"]+5,
                ifelse(StBdata[,"SB6_5"] %in% 11:13, StBdata[,"SB6_5"]+6, 
                 ifelse(StBdata[,"SB6_5"] >13, 19, NA))))), NA)))))   


################################ Abbreviated IQ (ABIQ) ################################
# Based on sum NVIQ and VIQ. 
# Sum of scaled scores, routing:
sum_NV_V <- NVIQ+VIQ
# NB: In SPSS, NAs are treated as 0 when added to a value.
# In the SPSS syntax, a new variable 'Valid_IQ_score' is computed to 
# indicate subjects with no missings in either NVIQ or VIQ.
# Here, subjects with one missing in NVIQ or VIQ are coded "NA" in sum_NV_V.

# Alternatively: Recoding to resemble the variable generated in SPSS (if desireable...):
# NVIQ[is.na(NVIQ)] <- 0
# VIQ[is.na(VIQ)] <- 0
# sum_NV_V <- NVIQ+VIQ

sum_NV_V_val <- seq(2, 38)
ABIQ_val <- c(47, 50, 52, seq(55, 148, by=3), 150, 153)
PercRank_val <- c(rep(0.1, 4), 0.3, rep(1, 3), 2, 4, 5, 8, 12, 16, 
                  21, 27, 34, 42, 50, 58, 66, 73, 79, 84, 88, 92, 
                  95, 96, 98, 99, 99, 99.5, 99.7, rep(99.9, 4))
ABIQ <- c()
PercRank <- c()
for(i in 1:length(sum_NV_V_val)) {
  ABIQ[sum_NV_V==sum_NV_V_val[i]] <- ABIQ_val[i]
}
for(i in 1:length(sum_NV_V_val)) {
  PercRank[sum_NV_V==sum_NV_V_val[i]] <- PercRank_val[i]
}

################################ Working memory (WM) index ################################
# Based on sum NVWM and VWM.
# From table SB-5 Examiner's Manual. Mean=100. 
WMsum = NVWMS + VWMS
# Recoding to resemble the variable generated in SPSS (if desireable...):
# NVWMS[is.na(NVWMS)] <- 0
# VWMS[is.na(VWMS)] <- 0
# WMsum = NVWMS + VWMS
WMsum_val <- seq(2, 38)
WMindex_val <- c(seq(48, 63, 3), 65, 
                 seq(68, 89, 3), 91,
                 seq(94, 115, 3), 117,
                 seq(120, 141, 3), 143,
                 seq(146, 152, 3))
WMindex <- c()
for(i in 1:length(WMsum_val)) {
  WMindex[WMsum==WMsum_val[i]] <- WMindex_val[i]
}

################################
StBdata[,c("NVIQ", "VIQ", "NVWMS", "VWMS", "VOS", "ABIQ", "PercRank", "WMindex")]  <- data.frame(NVIQ, VIQ, NVWMS, VWMS, VOS, ABIQ, PercRank, WMindex)
StBoutcomes <- StBdata[,c("PREG_ID_299", "BARN_NR", "NVWMS", "VWMS", "ABIQ", "WMindex")]


#write.table(StBoutcomes, "ADHD_outcomevars_StB.txt", sep="\t", dec=".")







