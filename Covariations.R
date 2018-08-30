library(data.table)
library(rstanarm)
options(mc.cores = 4)


ss.matrix = function(x) {
  vars = colnames(x)
  par(mfrow = c(ncol(x),ncol(x)), mar = c(.5,.5,.5,.5),mgp=c(1.5,.7,0), tck=-.01)
  for (v1 in vars) {
    for (v2 in vars) {
      if (v1 != v2) {
        par(mar = c(.5,.5,.5,.5))
        smoothScatter(x[,get(v1)],x[,get(v2)],ylab = "", 
                      bty = "n",
                      xlab = "", xaxt = "n", yaxt = "n", main = "")
        abline(lm(as.formula(paste0(v2,"~",v1)),x),col = "red")
      } else {
        par(mar = c(1.25,.5,2,1),mgp=c(2,.25,0))
        hist(x[,get(v1)],
             main = paste0(v1, ", n=",sum(!is.na(x[,get(v1)]))),
             cex.main=1,ylab = "", xlab = "",
             yaxt = "n")
      }
    }
  }
  par(mfrow = c(11))
}

my_heatmap = function(x) {
  cm = cor(x,use = "pairwise.complete.obs")
  diag(cm) = NA
  b = round(max(range(cm,na.rm = T))*20)/20
  breaks = seq(-b,b,by = .05)
  ncol = (length(breaks)-1)/2
  heatmap.2(cm,
            trace = "n",
            breaks = breaks,
            col = c(rev(sapply(seq(.025,1,length = ncol),function(x) adjustcolor("red",alpha = x))),
                    sapply(seq(.025,1,length = ncol),function(x) adjustcolor("darkgreen",alpha = x))),
            margins = c(12, 12),
            dendrogram = "none",
            key.title = "Correlations-Key",
            key.xlab = "",
            key.ylab = NA,
            keysize = .25,
            key.par=list(mgp=c(1,.4,0), tck=-.01,bty = "n", mar = c(3,0,3,0)),
            lmat = rbind(c(0,4,0,0),c(0,3,3,3),c(2,1,1,1)),
            lhei = c(1,.1,5),
            lwid = c(.1,4/3,4/3,4/3),
            key.ytickfun = function(){return(list(at = 0, labels = c(""), col = "white"))})
}

load("masterfile_scores.Rdata")


vars = c("StB.ABIQ.S", "BNT.S", "NY.INHIB.Statue.S","CDT.S","STP.S","TT.A.errors")



MASTER_scores[,BRF.IH.SS := BRF.P.IH.SS + BRF.T.IH.SS]
MASTER_scores[,BRF.EC.SS := BRF.P.EC.SS + BRF.T.EC.SS]
MASTER_scores[,BRF.WM.SS := BRF.P.WM.SS + BRF.T.WM.SS]
MASTER_scores[,BRF.PO.SS := BRF.P.PO.SS + BRF.T.PO.SS]
MASTER_scores[,BRF.SH.SS := BRF.P.SH.SS + BRF.T.SH.SS]
MASTER_scores[,BRF.IQ := (scale(BRF.PO.SS) + scale(BRF.WM.SS) + scale(BRF.SH.SS) + scale(BRF.IH.SS))/-4]
MASTER_scores[,CDI.L.SS := CDI.L.P.SS + CDI.L.T.SS]
MASTER_scores[,EAS.P.SHYNESS.SS := EAS.P.SHYNESS.SS*-1]

new_vars = c(names(MASTER_scores)[grep("EAS",names(MASTER_scores))],
             "BRF.IQ","BRF.EC.SS",
             "CDI.L.SS")



my_data = MASTER_scores[,c(vars,new_vars,"PP.ADHD.SS"),with = F]
my_data[, age := as.numeric(scale(MASTER_scores[,Age_in_days]))]
for (k in 1:ncol(my_data)) my_data[[k]] = scale(my_data[[k]])
my_data$gender = as.numeric(MASTER_scores$Gender)
ss.matrix(my_data)
my_heatmap(my_data)

models = vector(mode = "list", length = 3)
models[[1]] = stan_glm(as.formula(paste0("PP.ADHD.SS ~ ",paste(vars,collapse = " + "))),data = my_data,family = "gaussian")
models[[2]] = stan_glm(as.formula(paste0("PP.ADHD.SS ~ ",paste(new_vars,collapse = " + "))),data = my_data ,family = "gaussian")
models[[3]] = stan_glm(as.formula(paste0("PP.ADHD.SS ~ ",paste(c(new_vars,vars),collapse = " + "))),data = my_data,family = "gaussian")

plots = vector(mode = "list", length = 3)
for (m in 1:3) {
  plots[[m]] = plot(models[[m]])
  plots[[m]] = plots[[m]] + geom_vline(xintercept = 0)
  plots[[m]]
}


