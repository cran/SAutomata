#'
#'
#'
#'
#'
#'
#'
#'
Sbackward<-function(initsa,x,y,theta=NULL){

  n<-length(x)
  l<-length(initsa$states)
  if(is.null(theta)){
    scor_func<-scores(initsa)
  }else{
    scor_func<-scores(NULL,theta)
  }
  B<-matrix(data = NA, nrow = n+1,ncol = l)

  for(s in 1:l){
    B[n+1,s]<-0
  }
  for(k in n:1){
    for(s in l:1){
      B[k,s]<-Inf
      for(s_dash in l:1){
        score_col<-paste(toString(y[k]),toString(initsa$states[s]), sep=',')
        score_row<-paste(toString(x[k]),toString(initsa$states[s_dash]), sep=',')
        B[k,s]=min(B[k,s],(scor_func$scoress[score_row,score_col]+B[k+1,s_dash]))
      }
    }
  }
  colnames(B)<-initsa$states
  w<-Inf

  for(s in 1:l){
    w<-min(w[s],B[1,s])
  }
  return(B)
}
