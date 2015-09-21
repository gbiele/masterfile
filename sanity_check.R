

load("masterfile_scores.Rdata")

test_vars = names(MASTER_scores)[-grep("Age|Gender|PREG|BARN|VERSION|DIAG|.GR",names(MASTER_scores))]

MASTER_scores[,Age := scale(Age_in_days)]
MASTER_scores[,Gender := factor(gsub("ALE","",Gender))]
MASTER_scores[,ADHD := scale(PP.ADHD.SS)]
pdf(file = "sanity_check.pdf",width = 29/2.54,height = 21/2.54,pointsize = 10)
par(mfrow = c(5,4),mar = c(2,7,2,.5))
for (v in test_vars) {
  MASTER_scores[, y := scale(get(v))]
  f = lm(y~Age*Gender+ADHD,MASTER_scores)
  my_coefplot(f)
}
dev.off()


my_coefplot = function(fit){
  d = cbind(coef(fit),confint(fit),confint(fit,level = c(.9)))[-1,]
  clrs = rep("black",nrow(d))
  clrs[summary(f)$coefficients[-1,4] < .05] = "red"
  ys = 1:nrow(d)
  plot(d[,1],ys,
       xlim = range(d),
       ylim = c(0.5,nrow(d)+.5),
       xlab = "",ylab = "",
       yaxt = "n",
       pch = 19,
       col = clrs)
  segments(d[,2],ys,d[,3],ys,col = clrs)
  segments(d[,4],ys,d[,5],ys,lwd = 3,col = clrs)
  abline(v = 0,lty = 2,col = "black")
  axis(2,at = ys,labels = rownames(d),las = 2)
  mtext(side = 3, text = v,line = 0.2)
}