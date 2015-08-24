make_sum_scores = function(DT,items,ss_var){
  DT[,sNAs := sum(is.na(.SD)),by = 1:nrow(DT),.SDcols = items]
  DT[,sum_score := round(mean(as.numeric(.SD),na.rm = T)*length(items)),by = 1:nrow(DT),.SDcols = items]
  DT[sNAs >= (length(items)/2) , sum_score := NA,]
  setnames(DT,"sum_score",ss_var)
  return(DT)
}

char2num = function(x){
  if (class(x) != "numeric") {
    tmp = gsub("[a-z]|[A-Z]|[[:punct:]]"," ",x)
    options(warn = -1)
    x = as.numeric(tmp)
    options(warn = 0)
    chars = which(is.na(x) & tmp!= "")
    if(length(chars) > 0){
      for (v in chars){
        num_val = withCallingHandlers({
          val = as.numeric(strsplit(strsplit(tmp[v]," ")[[1]]," ")[[1]])
          val
        }, warning=function(w) {
          message("Warning: ", conditionMessage(w))
          message("sting was x: ", x)
          invokeRestart("muffleWarning")
        })
        x[v] = mean(num_val)
      }
    }
  }
  return(x)
}


library(fitdistrplus)

plot_my_hists = function(D){
  if (is(D,"data.table")) D = data.frame(D)
  for (v in colnames(D)) {
    d = D[,v]
    if (mean(quantile(d,c(.01,.99),na.rm = T)) < 
        median(d,na.rm = T)) {
      d = abs(d-max(d,na.rm = T))
      warning(paste("Variable",v,"reflected"),call = F)}
    if (is.numeric(d)) {
      nNA = sum(is.na(d))
      d = d[!is.na(d)]
      if (mean(unique(d) %% 1) == 0 & min(d)==0) {
        par(mfrow = c(3,2))
        plot_nbinom(d,v)
        plot_weibull(d,v)
        plot_gamma(d,v)
      } else if (mean(unique(d) %% 1) == 0 & min(d)>0) {
        par(mfrow = c(3,2))
        plot_nbinom(d,v)
        plot_weibull(d,v)
        plot_norm(d,v)
      } else if (min(d)>0) {
        par(mfrow = c(3,2))
        plot_norm(d,v)
        plot_weibull(d,v)
        plot_gamma(d,v)
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

plot_gamma = function(d,vname){
  d = d+.1
  f_gamma = fitdist(d,"gamma")
  max_val = max(max(d),qgamma(.999,shape = f_gamma$estimate["shape"], rate = f_gamma$estimate["rate"]))
  min_val = min(min(d),qgamma(.001,shape = f_gamma$estimate["shape"], rate = f_gamma$estimate["rate"]))
  
  h = hist(d,plot = F,breaks = seq(min_val-.5,max_val+.5,length = 15))
  
  predicted_density = dgamma(h$mids,shape = f_gamma$estimate["shape"],rate = f_gamma$estimate["rate"])
  predicted_density = predicted_density/sum(predicted_density)*sum(h$density)
  ttext = paste("log likelihood gamma:", round(f_gamma$loglik))
  hist(d,breaks = h$breaks,xlab = vname,main = ttext,ylim = c(0,max(h$density,predicted_density)),freq = F)
  lines(h$mids,predicted_density,'l',col = "blue")
  
  plot_delta_density(h$mids,predicted_density,h$density)
}

plot_weibull = function(d,vname){
  d = d+.1
  f_weibull = fitdist(d,"weibull")
  max_val = max(max(d),qweibull(.999,shape = f_weibull$estimate["shape"], scale = f_weibull$estimate["scale"]))
  min_val = min(min(d),qweibull(.001,shape = f_weibull$estimate["shape"], scale = f_weibull$estimate["scale"]))
  
  h = hist(d,plot = F,breaks = seq(min_val-.5,max_val+.5,length = 15))
  
  predicted_density = dweibull(h$mids,shape = f_weibull$estimate["shape"],scale = f_weibull$estimate["scale"])
  predicted_density = predicted_density/sum(predicted_density)*sum(h$density)
  ttext = paste("log likelihood weibull:", round(f_weibull$loglik))
  hist(d,breaks = h$breaks,xlab = vname,main = ttext,ylim = c(0,max(h$density,predicted_density)),freq = F)
  lines(h$mids,predicted_density,'l',col = "blue")
  
  plot_delta_density(h$mids,predicted_density,h$density)
}

plot_delta_density = function(x,predicted_density,oberved_density){
  delta_density = predicted_density - oberved_density
  clr = rep("red",length(delta_density))
  clr[delta_density > 0] = "green"
  plot(x,delta_density,"h",col = clr,lwd = 2,xlab = "",ylim = c(-.1,.1))
}


library(gplots)
library(RColorBrewer)
make_correlation_plot = function(D,save_image = T){
  cm = cor(D,use = "pairwise.complete.obs")
  if(save_image) png(filename = "heatmap.png",width = 35, height = 35, units = "cm", res = 600)
  heatmap.2(cm,
            breaks = seq(-1,1,length = 11),
            trace = "n",
            col = brewer.pal(n = 11,"PiYG")[-c(1)],
            margins = c(12,12))
  if(save_image) dev.off()
}
