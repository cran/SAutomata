#'
#'
#'
#'
#'
#'
#'
#'
initSA<- function(states,inputSymbols,outputSymbols,emissionProb,transitionProb){
    if(is.vector(states)&&is.vector(inputSymbols)&&is.vector(outputSymbols)&&is.matrix(emissionProb)&&is.matrix(transitionProb))
      {
      states<-as.character(states)
      csum<-0
      colnames(transitionProb) <- states[1:ncol(transitionProb)]
      rownames(transitionProb) <- states[1:nrow(transitionProb)]
      for(i in 1:nrow(transitionProb)){




        if( sum(transitionProb[,i]) !=1){

          return (stop("sum of transiton probability must be equal to 1"))
        }
      }

      mnames<-vector()
      c<-1
      for(i in 1:length(inputSymbols)){

        for(j in 1:length(states)){
          mnames[c]<-paste(toString(inputSymbols[i]),toString(states[j]), sep=',')
          c<-c+1
        }
      }

      csum<-0
      colnames(emissionProb) <- mnames[1:ncol(emissionProb)]
      rownames(emissionProb) <- outputSymbols[1:nrow(emissionProb)]

            for(j in 1:nrow(emissionProb)){


              if( sum(emissionProb[,j]) !=1){

          return (stop("sum of emision probability must be equal to 1"))
        }

      }
      return(list("states"=states,"inputSymbols"=inputSymbols,"outputSymbols"=outputSymbols,"transitionProb"=transitionProb,"emissionProb"=emissionProb))
    }else  {
      return(stop("Invalid arguments."))
      }
}
