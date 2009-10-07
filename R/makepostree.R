makepostree <-
function(treel,node,level,pos){
  treel[node,"level"] <- level
  treel[node,"position"] <- pos
  if(treel[node,"status"]== -3){
    ld <- treel[node,"left daughter"]
    rd <- treel[node,"right daughter"]
    treel <- makepostree(treel,ld,level+1,pos - 2^(-level-1))
    treel <- makepostree(treel,rd,level+1,pos + 2^(-level-1))
  }
  return(treel)
}

