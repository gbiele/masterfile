

## PAPA_K1.sav Play and Peer Relationships
## PAPA_K2.sav Sleep Behaviors (K_2_1..) Regulation/Habits (K_2_4..) 
## PAPA_K3.sav ADHD
## PAPA_K4.sav ODD/CD
## PAPA_K5.sav Anxiety
## PAPA_K6.sav Rituals and Repetitions (compulsion, OCD)
## PAPA_K7.sav Depression
## PAPA_K8.sav Tics
## PAPA_K9.sav/PAPA_K10.sav Life Events?

################################ SLEEP #########################################


SL = data.table(read_sav("savs//PAPA_K2.sav"))

old_names = c("K2_2_1_1_1", "K2_2_1_1_2", "K2_2_1_1_3","K2_2_1_2_1",
              "K2_2_1_3_1", "K2_2_1_3_2_1", "K2_2_1_3_2_2", "K2_2_1_3_2_3", "K2_2_1_3_2_4",
              "K2_2_1_4_1","K2_2_1_5_1","K2_2_1_6_1","K2_2_1_6_2",
              "K2_2_1_8_1","K2_2_1_8_2","K2_2_1_10_1","K2_2_1_11_1",
              "K2_2_1_12_1","K2_2_1_13_1","K2_2_1_14_1","K2_2_1_16_1")
new_names = c("got_to_bed","get_up","hours_per_night","resists_sleeping",
              "bedtime_ritual","bedtime_ritual.reading","bedtime_ritual.history","bedtime_ritual.singing","bedtime_ritual.other",
              "leaves_bed","wakes_at_night","time_to_fall_asleep","difficulties_sleeping",
              "self_sleeper_day","self_sleeper_evening","restless_sleep","not_rested_after_sleep",
              "Hypersomnia","sleeps_during_day","nightmares","somnabulism")
new_names = paste("PAPA.sleep.",new_names,sep = "")
setnames(SL,old_names,new_names)

SL = P2[,c(names(P2)[1:2],new_names),with = F]
rm(P2)

d = SL$PAPA.sleep.hours_per_night
d = gsub("[ a-z]","",d)
d = gsub(",",".",d)
d = gsub("??","",d)
for(k in  grep("-",d))  d[k] = mean(as.numeric(unlist(strsplit(d[k],"-"))))

d = as.numeric(d)
SL$PAPA.sleep.hours_per_night = d

t = SL$PAPA.sleep.time_to_fall_asleep
t = gsub("[ a-z]","",t)
t = gsub(",",".",t)
for(k in  grep("-",t))  t[k] = mean(as.numeric(unlist(strsplit(t[k],"-"))))
t = as.numeric(t)
t = SL$PAPA.sleep.time_to_fall_asleep
SL$PAPA.sleep.time_to_fall_asleep = t

################################ ADHD #########################################
AD = data.table(read_sav("savs/PAPA_K3.sav"))

AD[is.na(AD$K33_10_2), K33_10_2 := 0]
#AD$K33_10_1 = 2*(AD$K33_10_1 >= 2 | AD$K33_10_2 >= 2) # from SPSS syntax file "Diagnosegrupper PAPA_konkl juni2012"
AD$K33_10_1 = as.numeric(cut(AD$K33_10_2 + AD$K33_10_1,breaks = c(-1,1,3.5,6.5))) # modified version that preserves variance
AD$K33_10_1[AD$K33_10_1 < 2] = 0

items2dims = list(Hyp = c(1,3,4,5,7,8),
                  Imp = c(21,22,23),
                  ATT = c(10:18))
old_names = paste(paste("K33_",unlist(items2dims),sep = ""),"_1",sep = "")
new_names = paste("PAPA.ADHD.rating.",names(unlist(items2dims)),sep = "")
setnames(AD,old_names,new_names)
old_namesi = paste(paste("K33_",unlist(items2dims),sep = ""),"_2",sep = "")
new_namesi = paste("PAPA.ADHD.intensity.",names(unlist(items2dims)),sep = "")
old_namesi = gsub("K33_1_2","K33_1_3",old_namesi)
old_namesi = gsub("K33_3_2","K33_3_3",old_namesi)
old_namesi = gsub("K33_10_2","K33_10_4",old_namesi)
old_namesi = gsub("K33_22_2","K33_22_3",old_namesi)

setnames(AD,old_namesi,new_namesi)

AD = AD[,c(names(AD)[1:2],new_names,new_namesi),with = F]

################################ BEHAVIOR ######################################
BH = data.table(read_sav("savs/PAPA_K4.sav"))
#BH$K44_14_1 = 2*(BH$K44_14_1 > = 2 | BH$K44_15_1 >= 2) #(Collaps "erter" and "mobber" into variable K44_14).
#BH$K44_19_1 = 2*(BH$K44_19_1 > = 2 | BH$K44_20_1 >= 2) #(Collaps "sloss" and "angrep" into variable K44_19).
BH$K44_14_1 = as.numeric(cut(BH$K44_15_1 + BH$K44_14_1,breaks = c(-1,1,3.5,6.5)))
BH$K44_14_1[BH$K44_14_1<2] = 0
BH$K44_19_1 = as.numeric(cut(BH$K44_20_1 + BH$K44_19_1,breaks = c(-1,1,3.5,6.5)))
BH$K44_19_1[BH$K44_19_1<2] = 0
BH$K44_13_1[BH$K44_13_1 == 9] = NA # 9 was for "snakker ikke" in a question about lying

items2dims = list(ODD = c(3,4,6,8,9,10,11,12),
                  CD = c(13,14,16,17,18,19,21,22))

old_names = paste(paste("K44_",unlist(items2dims),sep = ""),"_1",sep = "")
new_names = paste("PAPA.BH.rating.",names(unlist(items2dims)),sep = "")
setnames(BH,old_names,new_names)

BH = BH[,c(names(BH)[1:2],new_names),with = F]


################################ ANXIETY ######################################
AX = data.table(read_sav("savs/PAPA_K5.sav"))

# 1. PHOBIA

items2dims = list(PHOB = 2:8,
                  SOC = 9:11,
                  SEP = c(13:14,16:19))
item2dimsGAX = 1:6

old_names = c(paste(paste("K55_",unlist(items2dims),sep = ""),"_1",sep = ""),
              paste("K55_26D_",item2dimsGAX,sep = ""))
new_names = c(paste("PAPA.ANX.rating.",names(unlist(items2dims)),sep = ""),
              paste("PAPA.ANX.rating.GEN",item2dimsGAX,sep = ""))

setnames(AX,old_names,new_names)

