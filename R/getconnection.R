getconnection <-
function(Z,X,all=TRUE){
  tab <- 0*diag(length(Z))
  samp <- lapply(Z,getsamples,X)
  highl <-  abs(getI(Z,X,mode="mean")$I)

  for (sam in 1:nrow(highl)){
    ind <- which(  highl[sam,] !=0 )
    if(length(ind)>1){
      if(!all){
        matind <- cbind( ind[-length(ind)], ind[-1])
        tab[matind] <- tab[matind] +1
      }else{
        tab[ind,ind] <- tab[ind,ind]+1
      }
    }
  }
  return(tab)
 
}

