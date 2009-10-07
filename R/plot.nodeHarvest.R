plot.nodeHarvest <-
function(x,  XTEST=NULL, highlight=NULL, varnames= NULL, threshold=0.005, yoffset=0.12, labels="all",  cexfaclab=1, ...){

  Z <- x[["nodes"]]
  connection <- x[["connection"]]
  weight <- sapply(x[["nodes"]],attr,"weight")
  if(is.null(varnames)) varnames <- x[["varnames"]]
  cexfac <- 10
  colgrey <- rgb(0,0,0,0.33) 
  colline <- rgb(0.3,0.3,0.3,0.4)
  colred <- rgb(1,0,0,0.5)

  if(threshold>=0){
    rem <- which(weight< threshold)
    
    if(length(rem)>0){
      connection <- connection[-rem,-rem]
      Z <- Z[-rem]
      weight <- weight[-rem]
    }
    ord <- order(sapply(Z,attr,"n"))
    Z <- Z[ord]
    weight <- weight[ord]
    connection <- connection[ord,ord]
  }

  
 
  
  isgrey <- rep(TRUE,length(Z))
  if(!is.null(highlight) ){
    if(highlight < 0 ){
      highlZ <- which(sapply(Z,attr,"leaf"))
    }else{
      highlZ <- which( apply( abs(getI(Z,XTEST[highlight,,drop=FALSE])$I) ,2,sum)!=0 )
    }
    if(length(highlZ)>0)  isgrey[ highlZ] <- FALSE
  }
    

  allmean <- sapply(Z,attr,"mean")
  alln <-  rank(sapply(Z,attr,"n") + seq(0.1,0,length=length(Z)) ,ties="first" )
  rang <- range(allmean)
  ylim <- c( min(alln)*0.9 , max(alln)*1.1 ) 
  xtmp <- allmean 
  xlimall <- mean(xtmp) + 1.1 * (1+(cexfaclab-1)/2) * range(xtmp-mean(xtmp))
  yvec <- alln

  plot(xtmp , yvec, cex=cexfac*sqrt(weight/max(weight)), ylim=ylim, type="n", axes=FALSE,xlab="RESPONSE",ylab="SAMPLES",xlim=xlimall)
  
  atvec <- round(seq(1,length(alln),length=6))
  axis(2,at=atvec,labels= sapply(Z,attr,"n")[atvec]  )
  
  isroot <- rep(FALSE,length(alln))
  if(!is.null(connection) ){
    connection <- diag(1/diag(connection)) %*% connection
    diag(connection) <- 0
    connection[lower.tri(connection)] <- 0
    for (k in 1:nrow(connection)){
      propcontained <- connection[k,]
      maxval <- max(propcontained)
      choose <- which( propcontained>=0.99999 )
      if(length(choose)>=1){
        choose <- choose[1]
        if(propcontained[choose]>0.99999) {
          co <- c(xtmp[k] , yvec[k])
          coc <- c(xtmp[choose],yvec[choose])
          lines( (c(coc[1], co[1])), c(coc[2],co[2]),col=colline,lwd=1)
        }else{
          isroot[k] <- TRUE
        }
      }
    }
  }
  points( xtmp , yvec ,cex=cexfac*sqrt(weight/max(weight)),col="white", pch=20)
  

  if(all(isgrey)){
    points( xtmp , yvec ,cex=cexfac*sqrt(weight/max(weight)),col=colgrey, pch=20)
  }else{
    indgrey <- which(isgrey)
    indred <- which(!isgrey)
    points( xtmp[indgrey] , yvec[indgrey] ,cex=cexfac*sqrt(weight[indgrey]/max(weight)),col=colgrey, pch=20)
    points( xtmp[indred] , yvec[indred] ,cex=cexfac*sqrt(weight[indred]/max(weight)),col=colred, pch=20)
  }

  axis(1)

  if(labels!="none"){
    drawlab <- if(labels=="all") 1:length(Z) else which(!isgrey)
    for (k in drawlab){
      if (weight[k]>threshold*max(weight)){
          drawtext(Z,k,offset= 0.00*abs(diff(xlimall)),varnames=varnames ,yoffset=1*yoffset,cex=0.55 * cexfaclab)    
      }
    }
  }
    
   
}

