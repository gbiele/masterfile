
adQuote <- function(x) paste("\"", x, "\"", sep = "")

writeSPSSfromLabelled <- function(df, datafile, codefile, varnames = NULL){
  df = data.frame(df)
  
  char_var = which((sapply(df,is.character)))
  
  if(length(char_var) > 0) {
    df = df[,-which((sapply(df,is.character)))]
    print(paste("removed following character variables: ",names(char_var)) )
    }
  
  ## FIXME: re-write this to hold a connection open
  dfn <- lapply(df, function(x) if (is.factor(x) | length(attr(x,"labels")) > 0) as.numeric(x) else x)
  write.table(dfn, file = datafile, row.names = FALSE, col.names = FALSE,
              sep = ",", quote = FALSE, na = "",eol = ",\n")
  
  varlabels <- names(df)
  varlabels_vars <- sapply(df,function(x) length(attr(x,"label")) > 0)
  for (k in which(sapply(df,function(x) length(attr(x,"label")) > 0))) {
    varlabels[k] = attr(df[,k],"label")
  }
  
  if (is.null(varnames)) {
    varnames <- abbreviate(names(df), 16)
    if (any(sapply(varnames, nchar) > 16))
      stop("I cannot abbreviate the variable names to eight or fewer letters")
    if (any(varnames != varlabels))
      warning("some variable names were abbreviated")
  }
  
  varnames <- gsub("[^[:alnum:]_\\$@#]", "\\.", varnames)
  
  dl.varnames <- varnames
  if (any(chv <- sapply(df,is.character))) {
    lengths <- sapply(df[chv],function(v) max(nchar(v)))
    if(any(lengths > 255L))
      stop("Cannot handle character variables longer than 255")
    lengths <- paste0("(A", lengths, ")")
    # corrected by PR#15583
    star <- ifelse(c(TRUE, diff(which(chv) > 1L))," *", " ")
    dl.varnames[chv] <- paste(star, dl.varnames[chv], lengths)
  }
  
  cat("DATA LIST FILE=", adQuote(datafile), " free (\",\")\n",
      file = codefile)
  cat("/",  dl.varnames, " .\n\n", file = codefile, append = TRUE)
  cat("VARIABLE LABELS\n", file = codefile, append = TRUE)
  cat(paste(varnames, adQuote(varlabels),"\n"), ".\n",
      file = codefile, append = TRUE)
  
  for (v in which(unlist(lapply(df,is.factor)))) {
    labels = 1:length(levels(df[,v]))
    names(labels) = levels(df[,v])
    df[,v] = labelled(as.integer(df[,v]),labels = labels) 
  }
  
  value_label_vars <- sapply(df,function(x) length(attr(x,"labels")) > 0)
  if (any(value_label_vars)) {
    cat("\nVALUE LABELS\n", file = codefile, append = TRUE)
    for(v in which(value_label_vars)){
      cat("/\n", file = codefile, append = TRUE)
      cat(varnames[v]," \n", file = codefile, append = TRUE, sep = "")
      levs <- names(attr(df[[v]],"labels"))
      cat(paste(seq_along(levs), adQuote(levs), "\n", sep = " "),
          file = codefile, append = TRUE)
    }
    cat(".\n", file = codefile, append = TRUE)
  }
  cat("\nEXECUTE.\n", file = codefile, append = TRUE)
}