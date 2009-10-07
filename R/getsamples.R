getsamples <-
function(mat,X){
  n <- nrow(X)
  ind <- rep(TRUE,n)
  for (k in 1:nrow(mat)){
    var <- mat[k,"variable"]
    if(!is.inf((mat[k,"lower"]))) ind <- ind & (X[,var]> mat[k,"lower"])
    if(!is.inf((mat[k,"upper"]))) ind <- ind & (X[,var]<= mat[k,"upper"])
  }
  if(any(is.na(ind))) ind[which(is.na(ind))] <- FALSE
  ind <- which(ind)
  return(ind)
}

