
miss = (colSums(is.na(MASTER_scores[,names(MASTER_scores)[-grep("^ECI|^CS|SDQ",names(MASTER_scores))],with = F])))
miss = miss[miss > 0]
par(mfrow = c(1,2))
par(mar = c(5,5,2,1))
hist(miss[miss > 0],breaks = seq(0,500,by = 10),main = "",
     ylab = "number of variables",
     xlab = "number missings data")
par(mar = c(5,10,2,1))
barplot(sort(miss[miss > 100]),horiz = T,las = 1,
        xlab = "number missings data")
par(mar = c(3,2,2,1))

hist_by_version(MASTER_scores[,grep("VERSION|ADHD.SS",names(MASTER_scores)),with = F])

MASTER_scores[,ECICS.P.ADHD.SS := sum(.SD,na.rm = T)/2,by = 1:nrow(MASTER),.SDcols = c("ECI.P.ADHD.SS","CS.P.ADHD.SS")]
MASTER_scores[,ECICS.T.ADHD.SS := sum(.SD,na.rm = T)/2,by = 1:nrow(MASTER),.SDcols = c("ECI.T.ADHD.SS","CS.T.ADHD.SS")]
avs = names(MASTER_scores)[grep("ADHD.SS",names(MASTER_scores))]
avs = avs[-grep("^CS\\.|^ECI\\.",avs)]
splom(MASTER_scores[,avs,with = F], panel = panel.smoothScatter, raster = TRUE)
cor(MASTER_scores[,avs,with = F],use = "pairwise.complete.obs")


library(MASS)
library(data.table)
library(ggplot2)
library(pscl)
library(boot)
load("masterfile.Rdata")
MASTER[,ECICS.P.ADHD.SS := round(sum(.SD,na.rm = T)/2),by = 1:nrow(MASTER),.SDcols = c("ECI.P.ADHD.SS","CS.P.ADHD.SS")]
MASTER[,ECICS.T.ADHD.SS := round(sum(.SD,na.rm = T)/2),by = 1:nrow(MASTER),.SDcols = c("ECI.T.ADHD.SS","CS.T.ADHD.SS")]

predictors = c("FD.ADHD","SB.NVIQ.S" , "SB.VIQ.S" , "CDT.S" , 
               names(MASTER)[grepl("^BRF",names(MASTER)) & grepl("SS$",names(MASTER))])
predictors = predictors[-grep("PO",predictors)]
criterion = "PP.ADHD.SS"


log_reflect = function(x){
  x = log(1+abs(x-max(x,na.rm = T)))
  return(abs(x-max(x,na.rm = T)))
}

Data = MASTER[,c(criterion,predictors),with = F]
Data[, (predictors) := lapply(.SD,scale), .SDcols = predictors]

sData = MASTER[,c(criterion,predictors),with = F]
sData[,CDT.S := log_reflect(CDT.S)]
logged_vars = names(sData)[grep("^FD|BRF",names(sData))]
sData[,(logged_vars) := lapply(.SD,function(x) log(1+x)), .SDcols = logged_vars]

rm(MASTER)


reg_model = as.formula(paste(criterion,"~", paste(predictors,collapse = " + ")))

lfit = lm(reg_model, data = Data)
nbfit = glm.nb(reg_model, data = Data)
zinbfit = zeroinfl(reg_model, data = Data,dist = "negbin")
s_zinbfit = zeroinfl(reg_model, data = sData,dist = "negbin")

### show posterior predictions ###

zero_beta = coef(zinbfit)[grep("zero",names(coef(zinbfit)))]
X = model.matrix(zinbfit$formula,Data)
count_beta = coef(zinbfit)[grep("count",names(coef(zinbfit)))]
p_zero = inv.logit(X %*% zero_beta)
mu = exp(X %*% count_beta)

s_zero_beta = coef(s_zinbfit)[grep("zero",names(coef(s_zinbfit)))]
s_X = model.matrix(s_zinbfit$formula,Data)
s_count_beta = coef(s_zinbfit)[grep("count",names(coef(s_zinbfit)))]
s_p_zero = inv.logit(X %*% zero_beta)
s_mu = exp(X %*% count_beta)


sims = 250
p_lfit = p_nbfit= p_zinbfit = p_s_zinbfit = matrix(nrow = nrow(lfit$model),ncol = sims)
for (k in 1:sims) {
  p_lfit[,k] = rnorm(n = nrow(lfit$model),mean = predict(lfit), sd = summary(lfit)$sigma)
  p_nbfit[,k] = rnbinom(n = nrow(nbfit$model),mu = exp(predict(nbfit)), size = nbfit$theta)
  
  p_zinbfit[,k] = rnbinom(n = length(mu),mu = mu, size = zinbfit$theta)
  p_zinbfit[which(p_zero > runif(length(p_zero))),k] = 0
  
  p_s_zinbfit[,k] = rnbinom(n = length(mu),mu = mu, size = s_zinbfit$theta)
  p_s_zinbfit[which(p_zero > runif(length(p_zero))),k] = 0
}

d = c(as.vector(p_lfit),as.vector(p_nbfit),as.vector(p_zinbfit),lfit$model[,1])
breaks = floor(min(d,na.rm = T)):max(d,na.rm = T)
h_data = hist(lfit$model[,1],breaks = breaks,plot = F)
plot_data = function(fname) {
  png(filename = fname,width = 20, height = 15,units = "cm", pointsize = 14, res = 150)
  par(mfrow = c(1,1),mar = c(4,4,.5,.5))
  plot(h_data$mids,h_data$counts,"s",
       lwd = 2,
       xlim = quantile(d,c(0,.99),na.rm = T),
       ylim = c(0,300),
       ylab = "count",
       xlab = "ADHD sum score")
  legend("topright", lwd = 2,
         legend = c("data","linear model","nb model", "zinb model"),
         col = c("black","red","blue","green"),bty = "n")
  }

plot_data("ppd_data.png")
dev.off()


plot_data("ppd_linear.png")
for (k in 1:sims) {
  h_lfit = hist(p_lfit[,k],breaks = breaks,plot = F)
  lines(h_data$mids,h_lfit$counts,"s",col = adjustcolor("red",alpha = .05))
}
dev.off()

plot_data("ppd_negbin.png")
for (k in 1:sims) {
  h_lfit = hist(p_lfit[,k],breaks = breaks,plot = F)
  lines(h_data$mids,h_lfit$counts,"s",col = adjustcolor("red",alpha = .05))
  h_nbfit = hist(p_nbfit[,k],breaks = breaks,plot = F)
  lines(h_data$mids,h_nbfit$counts,"s",col = adjustcolor("blue",alpha = .05))
}
dev.off()

plot_data("ppd_zinegbin.png")
for (k in 1:sims) {
  h_lfit = hist(p_lfit[,k],breaks = breaks,plot = F)
  lines(h_data$mids,h_lfit$counts,"s",col = adjustcolor("red",alpha = .05))
  h_nbfit = hist(p_nbfit[,k],breaks = breaks,plot = F)
  lines(h_data$mids,h_nbfit$counts,"s",col = adjustcolor("blue",alpha = .05))
  h_zinbfit = hist(p_zinbfit[,k],breaks = breaks,plot = F)
  lines(h_data$mids,h_zinbfit$counts,"s",col = adjustcolor("green",alpha = .05))
}
dev.off()

plot_data("ppd_s_zinegbin.png")
for (k in 1:sims) {
  h_lfit = hist(p_lfit[,k],breaks = breaks,plot = F)
  lines(h_data$mids,h_lfit$counts,"s",col = adjustcolor("red",alpha = .05))
  h_nbfit = hist(p_nbfit[,k],breaks = breaks,plot = F)
  lines(h_data$mids,h_nbfit$counts,"s",col = adjustcolor("blue",alpha = .05))
  h_zinbfit = hist(p_zinbfit[,k],breaks = breaks,plot = F)
  lines(h_data$mids,h_zinbfit$counts,"s",col = adjustcolor("green",alpha = .05))
  h_s_zinbfit = hist(p_s_zinbfit[,k],breaks = breaks,plot = F)
  lines(h_data$mids,h_s_zinbfit$counts,"s",col = adjustcolor("magenta",alpha = .05))
}
dev.off()

##############################
###### plot coefficients #####
##############################

pci_lfit = cbind(coef(lfit),confint(lfit))
pci_nbfit = cbind(coef(nbfit),confint(nbfit))
pci_zinbfit = cbind(coef(zinbfit)[grep("count",names(coef(zinbfit)))], 
                    confint(zinbfit)[grep("count",rownames(confint(zinbfit))),])
pci_s_zinbfit = cbind(coef(s_zinbfit)[grep("count",names(coef(s_zinbfit)))], 
                    confint(s_zinbfit)[grep("count",rownames(confint(s_zinbfit))),])

# apply exponential transform because zeroinfl uses log(mu) 
pci_nbfit[predictors,] = exp(pci_nbfit[predictors,]+pci_nbfit[1,1])-exp(pci_nbfit[1,1])
pci_nbfit[1,] = exp(pci_nbfit[1,])
pci_zinbfit[ paste0("count_",predictors),] = exp(pci_zinbfit[ paste0("count_",predictors),]+pci_zinbfit[1,1])-exp(pci_zinbfit[1,1])
pci_zinbfit[1,] = exp(pci_zinbfit[1,])
pci_s_zinbfit[ paste0("count_",predictors),] = exp(pci_s_zinbfit[ paste0("count_",predictors),]+pci_s_zinbfit[1,1])-exp(pci_s_zinbfit[1,1])
pci_s_zinbfit[1,] = exp(pci_s_zinbfit[1,])


png(filename = "beta_comps.png",width = 18, height = 15,units = "cm", pointsize = 16, res = 150)
lom = matrix(1,ncol = 2,nrow = 5)
lom[5,] = 2
layout(lom)
par(mar = c(3,8,.2,.2))

par_list = list(l = pci_lfit, nb = pci_nbfit, zinb = pci_zinbfit, s_zinb = pci_s_zinbfit)
clrs = c(l = "red", nb = "blue", zinb = "green", s_zinb = "magenta")
ofs =  seq(-.2,.2,length = 4)
names(ofs)

xlims = range(rbind(pci_lfit[-1,],pci_nbfit[-1,],pci_zinbfit[-1,]))
plot(1,type="n", xlim = xlims, ylim = c(0.5,length(predictors)+.5),
     yaxt = "n",
     ylab = "",
     xlab = "parameter value")
axis(2,at = 1:length(predictors), 
     labels =  gsub(".S$|.SS$","",predictors),
     las = 2)
abline(v = 0,lty = 2)

for (m in names(par_list)) {
  yx = (1:length(predictors)) + ofs[which(m == names(par_list))]
  points(par_list[[m]][-1,1],yx,col = clrs[m],pch = 19)
  segments(par_list[[m]][-1,2],yx,
           par_list[[m]][-1,3],yx,
           col = clrs[m],
           lwd = 2)
}

legend("topright", lwd = 2,
       legend = c("linear model","nb model", "zinb model"),
       col = c("red","blue","green"),bty = "n")

xlims = c(0,max(rbind(pci_lfit[1,],pci_nbfit[1,],pci_zinbfit[1,])))
plot(1,type="n", xlim = c(0,6), ylim = c(.75,1.25),
     yaxt = "n",
     xlab = "parameter value",
     ylab = "")
axis(2,at = 1, labels =  "Intercept", las = 2)
abline(v = 0,lty = 2)
for (m in names(par_list)) {
  yx = 1 + ofs[m]
  points(par_list[[m]][1,1],yx,col = clrs[m],pch = 19)
  segments(par_list[[m]][1,2],yx,
           par_list[[m]][1,3],yx,
           col = clrs[m],
           lwd = 2)
}
dev.off()