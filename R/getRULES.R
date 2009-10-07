getRULES <-
function(tree,RULES,maxinter=2){


  norules <- sum( ind <- ((tree[,"level"]<= maxinter + 1) & (tree[,"level"] > 1)))
  RULES <- vector("list",norules)
  loop <- which( ind )
  for (rc in 1:norules){

    ruletmp <- matrix( nrow= tree[ loop[rc], "level"]-1, ncol=3)
    colnames(ruletmp) <- c("variable","lower","upper")
    tree[,"split point"] <- signif(tree[,"split point"],3)

    node <- loop[rc]
    depth <- tree[ loop[rc],"level"]
    while( depth >1){
      nodenew <- which( apply( tree[1:(node-1),c("left daughter","right daughter"),drop=FALSE]==node,1,sum) >=1 )
      left <- tree[nodenew,"left daughter"]==node
      depth <- depth-1
      ruletmp[ depth,] <- c(tree[nodenew,"split var"], if(left) -Inf else tree[nodenew,"split point"], if(left) tree[nodenew,"split point"] else Inf)
      node <- nodenew
    }

    while(any( (tab <- table( ruletmp[,"variable"]))>1)){
      sel <- as.numeric( names( which(tab>1)[1]))[1]
      ind <- which( ruletmp[,"variable"]==sel)
      ruletmp[ ind[1], ] <- c( sel, max(ruletmp[ind,"lower"]),min(ruletmp[ind,"upper"]))
      ruletmp <- ruletmp[ -ind[2:length(ind)], ,drop=FALSE]
    }
    

    RULES[[rc]] <- ruletmp
    
    vari <- ruletmp[,c("lower","upper")]
    attr(RULES[[rc]],"depth") <- sum( abs(vari) < Inf   )
    
  }
  return(RULES)
    
  
}

