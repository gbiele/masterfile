library(haven)
library(plyr)
library(car)
library(stringr)
library(foreign)
library(data.table)

NaN2NA = function(DT) {
  for (j in names(DT))
    set(DT,which(is.na(DT[[j]])),j,NA)
  return(DT)
}

make_sum_scores = function(DT,items,ss_var,count_score = F,count_cutoff_idx = 1){
  DT$sNAs = 0L
  DT[,sNAs := sum(is.na(.SD)),by = 1:nrow(DT),.SDcols = items]
  tmp_items = paste0(items,".tmp")
  DT[,(tmp_items):=lapply(.SD, function(col) as.numeric(factor(col))-1),.SDcols = items]
  
  #DTi = smart_impute(DT[,tmp_items,with = F])
  #DT[, sum_score := rowSums(DTi)]
  DT[, sum_score := sum(.SD), by = 1:nrow(DT), .SDcols = items]
  
  DT[sNAs >= (length(items)/2) , sum_score := NA,]
  setnames(DT,"sum_score",ss_var)
  
  if (count_score){
    count_cutoff = sort(unique(unlist(lapply(DT[,items,with = F],
                                             unique))))[count_cutoff_idx]
    #DT[, count_score := sum(.SD > count_cutoff,na.rm = T),by = 1:nrow(DT), .SDcols = tmp_items]
    #DT[, count_score := round(count_score/(length(tmp_items)-sNAs)*length(tmp_items))]
    DT$count_score = rowSums(DT[,items,with = F])
    setnames(DT,"count_score",sub(".SS",".SC",ss_var))
  }
  
  DT = DT[,-which(names(DT) %in% c("sNAs",tmp_items)),with = F]
  #png(filename = paste0("IRT_FA/",gsub("\\.","_",ss_var),".png"),units = "cm", width = 30, height = 20, res = 150)
  #irt.fa(DT[,items,with = F])
  #dev.off()
  return(DT)
}

smart_impute = function(dt,items = NULL) {
  if(is.null(items))
    items =  names(dt) %in% grep("i[0-9]",names(dt), value = T)
  dt_complete = dt[rowSums(is.na(dt[,items,with = F])) == 0,]
  dt_missing = which(rowSums(is.na(dt[,items,with = F])) > 0 & rowSums(is.na(dt[,items,with = F])) < (sum(items)/2))
  
  for (k in dt_missing) {
    idx = as.vector(!(is.na(dt[k]))) & items
    
    mat = as.matrix(dt_complete[,idx,with = F])
    rvec = as.matrix(dt[k,idx,with = F])
    d = apply(mat,
              1,
              function(x) dist(rbind(x,rvec)))
    
    impvars = intersect(names(dt)[!idx],
                        names(dt)[items])
    imp = round(colMeans(dt_complete[d == min(d),
                                     impvars,
                                     with = F]
                         )
                )
    for (v in impvars)
      dt[[v]][k] = imp[v]
  }
  return(dt)
}

add_label = function(dt,prefix,abbreviations,my_warning = T) {
  for (v in  names(dt)[grep(paste0("^",prefix),names(dt))]) {
    tmp_abbrev = abbreviations
    short_name = strsplit(v,"\\.")[[1]]
    
    if (length(grep("i[0-9]",short_name)) > 0) {
      tmp = short_name[grep("i[0-9]",short_name)]
      tmp_abbrev[tmp] = paste("item",sub("i","",tmp))}
    
    if (any(!short_name %in% names(tmp_abbrev))){
      for (n in short_name[!short_name %in% names(tmp_abbrev)]) {
        tmp_abbrev[n] = n
        if (my_warning & nchar(n)<4) print(paste0("Missing long name for ",n," +++",v,"+++"))
        }
    }
    
    if (sum(!is.na(tmp_abbrev[short_name])) == length(short_name)) {
      long_name = paste(tmp_abbrev[short_name],collapse = "; ")
      
      orig_label = attributes(dt[[v]])[["label"]]
      if(length(orig_label) > 0 && !is.na(orig_label)) {

        orig_label = strsplit(attributes(dt[[v]])$label,";")[[1]]
        if(orig_label[length(orig_label)] == " .") orig_label = orig_label[1:(length(orig_label)-1)]
        orig_label = orig_label[length(orig_label)]
        if (nchar(orig_label) < 5) {
          orig_label = ""
        } else {
          if (length(grep(";",attr(dt[[v]],"label"))) > 0) {
            orig_label = paste0(" [Orig Label: ",orig_label,"]")
          } else {
            orig_label = paste0("; ",orig_label)
          }
        }
        long_name = paste0(long_name,orig_label)
      }
    long_name = gsub(" . ","",long_name)
      if ("labels" %in% names(attributes(dt[[v]]))) {
        attributes(dt[[v]])$label = long_name
      } else {
        attributes(dt[[v]]) = list(label = long_name)
      }
      
    } else {
      stop(paste("did not find all translations for",v))
    }
  }
  return(dt)
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


hist_by_version = function(D){
  par (mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01)
  par(mfrow = c(4,5))
  if (is(D,"data.table")) D = data.frame(D)
  for (v in colnames(D)) {
    if (is.numeric(D[,v])){
      h = hist(D[,v],plot = F)
      
      x = sort(c(h$mids,range(h$breaks)))
      y1 = c(0,hist(D[D$VERSION == 1,v],breaks = h$breaks,plot = F)$counts,0)
      y2 = c(0,hist(D[D$VERSION == 2,v],breaks = h$breaks,plot = F)$counts,0)
      plot(x,y1,'s',
           main = v,
           ylab = "density",
           xlab = "value",
           ylim = c(0,max(c(y1,y2,sum(is.na(D[,v]))))))
      lines(x,y2,'s',col = "magenta")
      lines(rep(mean(c(par("usr")[2],par("xaxp")[1:2][2])),2),c(0,sum(is.na(D[,v]))),lwd = 2, col = "red")
      }
    }
  par(mfrow = c(1,1))
}
  

shist_by_version = function(D){
  par (mar=c(0.1,0.1,0.1,0.1), mgp=c(2,.7,0), tck=-.01)
  par(mfrow = c(11,13))
  if (is(D,"data.table")) D = data.frame(D)
  for (v in setdiff(colnames(D),c(index_vars,"VERSION"))) {
    h = hist(D[,v],plot = F)
    
    x = sort(c(h$mids,range(h$breaks)))
    y1 = c(0,hist(D[D$VERSION == 1,v],breaks = h$breaks,plot = F)$counts,0)
    y2 = c(0,hist(D[D$VERSION == 2,v],breaks = h$breaks,plot = F)$counts,0)
    plot(x,y1,'s',
         main = "",
         ylab = "",
         xlab = "",
         ylim = c(0,max(y1,y2)),
         xaxt = "n",
         yaxt = "n",
         bty = "n")
    axis(1,at = range(h$mids),labels = c("",""))
    lines(x,y2,'s',col = "red")
  }
  par(mfrow = c(1,1))
}


hists = function(D){
  if (is.data.table(D)) D = data.frame(D)
  par (mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01)
  par(mfrow = c(4,5))
  for (v in colnames(D)) {
    if (is.numeric(D[,v])){
      hist(D[,v],main = v)
    }
  }
}



library(fitdistrplus)

plot_my_hists = function(D){
  if (is(D,"data.table")) D = data.frame(D)
  for (v in setdiff(colnames(D),c(index_vars,"VERSION"))) {
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


get_time = function(T){
  nT = rep(NA,length(T))
  for (k in 1:length(T)) {
    if (nchar(T[k])<=3 & length(grep("-|:|,|\\.",T[k])) == 0){
      nT[k] = as.numeric(T[k])*60
    } else if (length(grep(":|,|\\.",T[k])) > 0 & length(grep("-",T[k])) > 0) {
      t1 = make_minutes(strsplit(T[k],"-")[[1]][1])
      t2 = make_minutes(strsplit(T[k],"-")[[1]][2])
      nT[k] = (t1+t2)/2
    } else if (length(grep(":|,|\\.",T[k])) > 0 & length(grep("-",T[k])) == 0) {
      nT[k] = make_minutes(T[k])
    }else if (length(grep(":|,|\\.",T[k])) == 0 & length(grep("-",T[k])) > 0) {
      nT[k] = sum(as.numeric(strsplit(T[k],"-")[[1]])*60)/2
    } else if (length(grep("-|:|,|\\.",T[k])) == 0 & nchar(T[k]) == 4){
      nT[k] = sum(c(as.numeric(substr(T[k],1,2)),
                    as.numeric(substr(T[k],3,4))) *c(60,1))
    }
  }
  return(nT)
}

make_minutes = function(s){
  tt = sub(":|,|\\.","-",s)
  tt = as.numeric(strsplit(tt,"-")[[1]])
  if (length(tt) == 2) {
    mins = sum(tt * c(60,1))
  } else {
    mins = tt*60
  }
  return(mins)
}

make_numbers = function(s){
  if (s == "") {
    n = NA
  } else {
    tt = gsub(",",".",gsub("[a-z]| ","",s))
    tt = as.numeric(strsplit(tt,"-")[[1]])
    if (length(tt) == 2) {
      n = mean(tt)
    } else {
      n = tt
    }  
  }
  return(n)
}

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
