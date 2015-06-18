
plot_my_hists = function(D){
  if (is(D,"data.table")) D = data.frame(D)
  for (v in colnames(D)) {
    if (is.numeric(D[,v])) hist(D[,v],main = v)
    if (!is.numeric(D[,v]) & length(unique(D[,v]))<25) bar(table(addNA(D[,v])))
    par(ask=T) 
  }
  par(ask=F) 
}