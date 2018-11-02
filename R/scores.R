#'
#'
#' @title  Calculation of Probabilities (Not For End User)
#' @param initsa  Model SA.
#' @param  theta  Optional(Conditional Prababilities).
#' @usage  scores(initsa=NULL,theta=NULL)
#' @description This function is not for end user.
#' @examples
#' \dontrun{
#' scores(initsa)
#' }
#'
#'
#'
#'
scores<- function(initsa=NULL,theta=NULL){
if(is.null(theta)){
  #...................................

  scoress<-matrix(data=NA, nrow = length(initsa$states)*length(initsa$inputSymbols), ncol = length(initsa$states)*length(initsa$outputSymbols) )
  theta<-scoress
  cnames<-is.vector(NULL)
  rnames<-is.vector(NULL)
  c<-1
  for(i in 1:length(initsa$inputSymbols)){

    for(j in 1:length(initsa$states)){
      cnames[c]<-paste(toString(initsa$inputSymbols[i]),toString(initsa$states[j]), sep=',')

      c<-c+1
    }
  }
  r<-1

  for(i in 1:length(initsa$outputSymbols)){

    for(j in 1:length(initsa$states)){
      rnames[r]<-paste(toString(initsa$outputSymbols[i]),toString(initsa$states[j]), sep=',')

      r<-r+1
    }
  }
  rownames(scoress) <- cnames[1:ncol(scoress)]
  colnames(scoress) <- rnames[1:ncol(scoress)]

  rownames(theta) <- cnames[1:ncol(theta)]
  colnames(theta) <- rnames[1:ncol(theta)]
  for(s in 1:length(initsa$states)){
  for(i in 1:length(initsa$inputSymbols)){

    rscoress<-paste(toString(initsa$outputSymbols[i]),toString(initsa$states[s]), sep=',')
      for(i_dash in 1:length(initsa$inputSymbols)){
        for(s_dash in 1:length(initsa$states)){
          ctp<-paste(toString(initsa$inputSymbols[i_dash]),toString(initsa$states[s_dash]), sep=',')
          is.na(scoress[ctp,rscoress])
          if(is.na(scoress[ctp,rscoress])&&is.element(ctp,colnames(initsa$emissionProb))){

           scoress[ctp,rscoress]<-round(-log(initsa$emissionProb[i,ctp]*initsa$transitionProb[s,s_dash]),2)
           theta[ctp,rscoress]<-round(initsa$emissionProb[i,ctp]*initsa$transitionProb[s,s_dash],2)
           }
          }
      }
    }
  }
    return(list("scoress"=scoress,"theta"=theta))
}else{
  return(list("scoress"=round(-log(theta),2),"theta"=theta))
}
}
