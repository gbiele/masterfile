make_sum_scores = function(item_scores){
  sNAs = apply(is.na(item_scores),1,sum)
  incomplete.idx = sNAs > (dim(item_scores)[2]/2)
  sum_scores = apply(item_scores,1,function(x) sum(x,na.rm = TRUE) + median(x,na.rm = TRUE) * sum(is.na(x)))
  sum_scores = round(sum_scores)
  sum_scores[incomplete.idx] = NA
  return(round(sum_scores))
}


library(fitdistrplus)

plot_my_hists = function(D){
  if (is(D,"data.table")) D = data.frame(D)
  for (v in colnames(D)) {
    d = D[,v]
    if (is.numeric(d)) {
      nNA = sum(is.na(d))
      d = d[!is.na(d)]
      if (mean(unique(d) %% 1) == 0) {
        par(mfrow = c(2,2))
        plot_nbinom(d,v)
        plot_norm(d,v)
      } else {
        par(mfrow = c(1,2))
        plot_norm(d,v)
      }
      info_string = paste(v,", ",round(nNA/nrow(D),digits = 2)*100,"% missing",
                          sep = "")
      mtext(info_string,side = 3,line = 4,cex = 1.5,outer = T)
    }
    if (!is.numeric(d) & length(unique(d))<25) bar(table(addNA(d)))
    par(ask=T) 
  }
  par(ask=F) 
}

plot_nbinom = function(d,vname) {
  f_nb = fitdist(d,"nbinom")
  max_val = max(max(d),qnbinom(.99,mu = f_nb$estimate["mu"],size = f_nb$estimate["size"]))
  predicted_density = dnbinom(0:max_val,mu = f_nb$estimate["mu"],size = f_nb$estimate["size"])
  oberved_density = apply(data.frame(0:max_val),1,function(x) sum(d %in% x))/length(d)*sum(predicted_density)
  ttext = paste("log likelihood nbinom:", round(f_nb$loglik))
  plot(0:max_val,oberved_density,"h",xlab = vname,ylab = "density",ylim = c(0,max(predicted_density,oberved_density)),main = ttext)
  lines((0:max_val)+.2,predicted_density,"h",col = "red")
  
  plot_delta_density(0:max_val,predicted_density,oberved_density)
}

plot_norm = function(d,vname){
  f_norm = fitdist(d,"norm")
  max_val = max(max(d),qnorm(.999,mean = f_norm$estimate["mean"],sd = f_norm$estimate["sd"]))
  min_val = min(min(d),qnorm(.001,mean = f_norm$estimate["mean"],sd = f_norm$estimate["sd"]))
  
  h = hist(d,plot = F)
  vals = sort(unique(c(h$breaks,h$breaks-max(h$breaks),h$breaks + max(h$breaks))))
  vals = vals[rev(which(vals<min_val))[1]:which(vals>max_val)[1]]
  h = hist(d,breaks = vals,plot = F)
  predicted_density = dnorm(h$mids,mean = f_norm$estimate["mean"],sd = f_norm$estimate["sd"])
  predicted_density = predicted_density/sum(predicted_density)*sum(h$density)
  ttext = paste("log likelihood norm:", round(f_norm$loglik))
  hist(d,breaks = vals,xlab = vname,main = ttext,ylim = c(0,max(h$density,predicted_density)),freq = F)
  lines(h$mids,predicted_density,'l',col = "blue")
  
  plot_delta_density(h$mids,predicted_density,h$density)
}

plot_delta_density = function(x,predicted_density,oberved_density){
  delta_density = predicted_density - oberved_density
  clr = rep("red",length(delta_density))
  clr[delta_density > 0] = "green"
  plot(x,delta_density,"h",col = clr,lwd = 2,xlab = "",ylim = c(-.1,.1))
}