nodeHarvest <-
function(X,Y, nodesize=10, nodes=1000, maxinter=1, mode="mean", lambda=Inf, addto=NULL,  onlyinter=NULL, silent=FALSE){

  if(is.data.frame(X)){
    colX <- colnames(X)
    for (k in 1:ncol(X)){
      if(class(X[,k])!="numeric"){
        if(!silent) cat("\n", paste("converting ",class(X[,k]), " variable `",colnames(X)[k],"' to numeric vector in current version ..."),sep="")
        X[,k] <- as.numeric(X[,k])
      }
    }
    X <- as.matrix(X)
    colnames(X) <- colX
  }

  
  imputed <- FALSE
  if(any(is.na(X))){
    
    if(!silent) cat("\n"," imputing missing values for node generation ...")
    tmp <- capture.output( X <- rfImpute(X,if( length(unique(Y))<= 5 ) as.factor(Y) else Y,iter=3)[,-1])
  }
  if(!silent) cat("\n ... generating",nodes,"nodes ...")
  Z <- makeRules( X ,Y,nodes=nodes,addZ= addto ,nodesize=nodesize, maxinter=maxinter+1, onlyinter=onlyinter, silent=silent)
  
  conn <- attr(Z,"connection")

  if(!silent) cat(" ... computing node means ...","\n")
  geti <- getI(Z,X,Y,mode=mode)
  I <- geti$I
  Z <- geti$Z
  
  wleafs <- rep(0,length(Z))
  indroot <- which(sapply(Z,attr,"depth")==0)[1]
  wleafs[indroot] <- 1
  
  if(!silent) cat(" ... computing node weights ...")
  w <- getw(I,Y,Isign=abs(sign(I)),wleafs=wleafs, epsilon=lambda-1,silent=silent)
    
  rem <- which(abs(w) < 10^(-3))
  if(length(rem)>0){
    Z <- Z[-rem]
    w <- w[-rem]
    conn <- conn[-rem,-rem,drop=FALSE]
  }
  for (k in 1:length(Z)) attr(Z[[k]],"weight") <- w[k]
  
  nh <- list()
  nh[["connection"]] <- conn
  nh[["varnames"]] <- colnames(X)
  nh[["predicted"]] <- as.numeric(I[,-rem,drop=FALSE] %*% w)
  nh[["nodes"]] <- Z
  nh[["Y"]] <- Y
  class(nh) <- "nodeHarvest"
  
  return(nh)
   
 }

