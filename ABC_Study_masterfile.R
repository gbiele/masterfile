setwd("F:/Forskningsprosjekter/PDB 299 - ADHD-studien Prescho_/Forskningsfiler/GUBI/GuidoData/masterfile/")

library(haven)
library(data.table)
source("scripts4tests/utils.R")

droot = "F:/Forskningsprosjekter/PDB 1467 - Psykiatriske symptom_/Forskningsfiler/Originalfiler/Originalfiler 2018/MoBa_v10_PDB1467/Data/"

stb = data.table(read_sav(paste0(droot,"PDB1467_ABC_analysefil_v10.sav")))

stb = stb[as_factor(stb$INVSTAT) == "Assessed"]
stb = stb[ASSAGED/365.25 < 4]


setnames(stb,paste0("E__1_4_",2:5),paste0("DIAGNOSIS",1:4))
stb[,ADHD := "No"]
for (v in paste0("DIAGNOSIS",1:4)) {
  stb[[v]] = as_factor(stb[[v]])
  idx = grep("ADHD",stb[[v]])
  stb[["ADHD"]][idx] = as.character(stb[[v]][idx])
} 
stb[,Diag.ADHD := ifelse(grepl("ADHD",ADHD),"Yes","No")]
stb[, Diag.MR := "No"]
stb[!is.na(E__1_3_1), Diag.MR := "Yes"]
stb[, Diag.ASD := "No"]
stb[!is.na(E__1_1_1), Diag.ASD := "Yes"]
stb[, Diag.ADHD.only := "No"]
stb[Diag.ADHD == "Yes" & Diag.MR == "No" & Diag.ASD == "No",Diag.ADHD.only := "Yes"]

setnames(stb,
         c("E__813_2","E__8_7_2","E__8_5_2","E__8_6_2","E__8_2_5","E__8_4_5","E__812_2","E_15_4_3"),
         c("StB.ABIQ.S","StB.TOTIQ.S","StB.NVIQ.S","StB.VIQ.S","StB.NVWMS.S","StB.VWMS.S","StB.WMindex.S","WASI.compl"))
setnames(stb,"E__1_1_2","ASD.Subtype")
stb[, ASD.Subtype := as.character(as_factor(ASD.Subtype))]
stb[is.na(ASD.Subtype), ASD.Subtype := "No"]

stb[StB.ABIQ.S < 40, StB.ABIQ.S := NA]
m = lm(StB.ABIQ.S ~ StB.TOTIQ.S, stb)
stb[!is.na(StB.TOTIQ.S) & is.na(StB.ABIQ.S),
    StB.ABIQ.S := round(predict(m,
                           newdata = data.frame(StB.TOTIQ.S)))]

stb[, IQ := StB.ABIQ.S]
stb[!is.na(WASI.compl) & is.na(StB.ABIQ.S), IQ := WASI.compl]

rm(m,idx,v)

# cdi, brief, cbq, eas

pq = data.table(read_sav(paste0(droot,"PDB1467_ABC3_v10.sav")))

cdii = paste0("SO",c(63:110,112,113))
setnames(pq,cdii,paste0("CDI.P.L.i",1:50))
for (v in paste0("CDI.P.L.i",1:50)) pq[[v]] = 1*(pq[[v]] > 1)
pq = make_sum_scores(pq,paste0("CDI.P.L.i",1:50), "CDI.P.L.SS")

briefi = paste0("SO",135:197)
brief_dims = data.table(read.csv2("F:/Forskningsprosjekter/PDB 299 - ADHD-studien Prescho_/Forskningsfiler/GUBI/GuidoData/masterfile/instrument_docs/brief_items4dims.txt",sep = ","))
setkeyv(brief_dims,"item")
base_name = "BRF.P."
setnames(pq,briefi,paste0(base_name,brief_dims[,dimension],".i",sprintf("%02d",brief_dims[,item])))
for (d in unique(brief_dims$dimension)){
  items = names(pq)[grep(paste0(".",d,"."),names(pq))]
  pq = make_sum_scores(pq,items, paste0(base_name,d,".SS"))
}


cbqi = grep("C_28",names(pq),value = T)
cbq_item_info = fread("F:/Forskningsprosjekter/PDB 299 - ADHD-studien Prescho_/Forskningsfiler/GUBI/GuidoData/masterfile/instrument_docs/cbq_items_and_scale.txt")
cbq_item_info[,item_direction := sign(item_number)]
cbq_item_info[,item_number := abs(item_number)]
setkey(cbq_item_info,"item_number")
empathy_items = data.table(item_number = 37:50, scale = "Empathy", item_direction = 1)
empathy_items[item_number %in% c(44,45,48,49), item_direction := -1]
cbq_item_info = rbind(cbq_item_info,empathy_items)

cbqitems = paste(paste0("CBQ.P.",cbq_item_info$scale),
                 cbq_item_info$item_number,
                 sep = ".i")

setnames(pq,cbqi[1:50],cbqitems )


## reflect some items
ritems = paste(paste0("CBQ.P.",
                      cbq_item_info[item_direction < 0,scale]),
               cbq_item_info[item_direction < 0,item_number],
               sep = ".i")
for ( i in ritems) {
  pq[[i]] = abs(pq[[i]]-8)
}

cbq_scales = unique(cbq_item_info$scale)
for (s in cbq_scales) {
  pq = make_sum_scores(pq,grep(s,names(pq)),paste0("CBQ.P.",s,".SS"))
}



eas_scales = list(EMOTION = c(1,7,10),
                  ACTIVITY = c(2,4,-8),
                  SHYNESS = c(-5,6,-11),
                  SOCIABIL = c(3,9,12))
scl = rep("n",length(unlist(eas_scales)))
for (s in names(eas_scales)) scl[abs(eas_scales[[s]])] = s

setnames(pq,cbqi[51:62],paste0("EAS.P.",scl,".i",1:12))

for (d in names(eas_scales)) {
  for (item in abs(eas_scales[[d]][eas_scales[[d]]<0])) {
    item_name = paste0("EAS.P.",d,".i",item)
    pq[[item_name]] = pq[[item_name]]*-1+max(pq[[item_name]],na.rm = T)+1
  }
}

for (s in names(eas_scales)) {
  pq = make_sum_scores(pq,grep(s,names(pq)),paste0("EAS.P.",s,sep = ".SS"))
}


md = merge(stb,pq,by = c("PREG_ID_1467", "BARN_NR"),all.x = T, all.y = T)
vars = unique(c(names(md)[1:23],grep("ADHD|Diag|DIAG|CBQ|EAS|CDI|BRF|StB|IQ",names(md),value = T)))
md = md[,vars,with = F]

rm(list = grep("md",ls(),invert = T, value = T))

load("masterfile_scores.Rdata")

MASTER_scores = MASTER_scores[StB.ABIQ.S > 70,]
md = md[StB.ABIQ.S > 70,]

library(rstanarm)
library(brms)

### ABIQ ###

y1 = md[Diag.ADHD.only == "Yes",StB.ABIQ.S]
y2 = MASTER_scores[!(is.na(StB.ABIQ.S)) & as_factor(PP.ADHD.GR) %in% c("ADHD_IA_clin ","ADHD_H_clin","ADHD_c_clin"), StB.ABIQ.S]
y1 = y1[!is.na(y1)]
y2 = y2[!is.na(y2)]

y = c(y1,y2)
x = c(rep(1,length(y1)),rep(0,length(y2)))
sf = stan_lm(y~x, data = data.frame(y,x),prior = NULL)
hist(as.matrix(sf$stanfit)[,"x"],
     main = "Difference in IQ of children with ADHD",
     xlab = "IQ ABC-Study minus IQ ADHD-Study")
abline(v = c(-5,5), col = "red")

### WM ###

y1 = md[Diag.ADHD.only == "Yes",StB.WMindex.S]
y2 = MASTER_scores[!(is.na(StB.WMindex.S)) & as_factor(PP.ADHD.GR) %in% c("ADHD_IA_clin ","ADHD_H_clin","ADHD_c_clin"), StB.WMindex.S]
y1 = y1[!is.na(y1)]
y2 = y2[!is.na(y2)]

y = c(y1,y2)
x = c(rep(1,length(y1)),rep(0,length(y2)))
sf = stan_lm(y~x, data = data.frame(y,x),prior = NULL)
hist(as.matrix(sf$stanfit)[,"x"],
     main = "Difference in WM of children with ADHD",
     xlab = "WM ABC-Study minus WM ADHD-Study")
abline(v = c(-5,5), col = "red")


### CDI ###

y1 = md[Diag.ADHD.only == "Yes",CDI.P.L.SS]
y2 = MASTER_scores[as_factor(PP.ADHD.GR) %in% c("ADHD_IA_clin ","ADHD_H_clin","ADHD_c_clin"), CDI.L.P.SS]
y1 = 50-y1[!is.na(y1)]
y2 = 50-y2[!is.na(y2)]

y = c(y1,y2)
x = c(rep(1,length(y1)),rep(0,length(y2)))


#custom family
beta_binomial2 <- custom_family(
  "beta_binomial2", dpars = c("mu", "phi"),
  links = c("logit", "log"), lb = c(NA, 0),
  type = "int", vars = "trials[n]")

stan_funs <- "
real beta_binomial2_lpmf(int y, real mu, real phi, int T) {
return beta_binomial_lpmf(y | T, mu * phi, (1 - mu) * phi);
}
int beta_binomial2_rng(real mu, real phi, int T) {
return beta_binomial_rng(T, mu * phi, (1 - mu) * phi);
}
"

predict_beta_binomial2 <- function(i, draws, ...) {
  mu <- draws$dpars$mu[, i]
  phi <- draws$dpars$phi
  N <- draws$data$trials[i]
  beta_binomial2_rng(mu, phi, N)
}

fitted_beta_binomial2 <- function(draws) {
  mu <- draws$dpars$mu
  trials <- draws$data$trials
  trials <- matrix(trials, nrow = nrow(mu), ncol = ncol(mu), byrow = TRUE)
  mu * trials
}

bf = brm( y ~ x, data = data.frame(y,x),
          family = beta_binomial2,
          stan_funs = stan_funs,
          prior = c(set_prior("normal(0,1)", class = "b"),
                    set_prior("normal(0,1)", class = "Intercept")),
          stanvars = stanvar(rep(50L,length(y)), name = "trials"))
expose_functions(bf, vectorize = TRUE)
pp = posterior_predict(bf)

nABC = length(y1)
nADHD = length(y2)

AME = rowMeans(pp[,x == 0]) - rowMeans(pp[,x == 1])

sc = "
data {
  int N;
  int Y[N];
}
parameters {
  real<lower = 0> alpha;
  real<lower = 0> beta;
}
model {
  alpha ~ normal(0,10);
  beta ~ normal(0,10);
  Y ~ beta_binomial(50,alpha,beta);
}
generated quantities {
  real bbsd;
  bbsd = ((50*alpha*beta*(alpha+beta+50))/( (alpha + beta)^2 * (alpha + beta + 1) ))^.5;
}

"
sm = stan_model(model_code = sc)
options(mc.cores = 4)
sf = sampling(sm,data = list(N = length(y), Y = y), iter = 2000)

hist(AME/summary(sf)$summary["bbsd","mean"],
     main = "Standardize Difference in CDI of children with ADHD",
     xlab = "SMD CDI ABC-Study minus CDI ADHD-Study")
abline(v = c(-.5,.5), col = "red")
