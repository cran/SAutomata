#'
#'
#'
#'
#'
#'
#'
#'
Sforward<-function(initsa,x,y,theta=NULL){

  n<-length(x)
  l<-length(initsa$states)
  if(is.null(theta)){
    scor_func<-scores(initsa)
  }else{
    scor_func<-scores(NULL,theta)
  }
  M<-matrix(data = NA, nrow = n+1,ncol = l)

  for(s in 1:l){
    M[1,s]<-0
  }
  for(k in 1:n){
    for(s in l:1){
      M[k+1,s]<-Inf
      for(s_dash in l:1){
        score_col<-paste(toString(y[k]),toString(initsa$states[s]), sep=',')
        score_row<-paste(toString(x[k]),toString(initsa$states[s_dash]), sep=',')
        M[k+1,s]=min(M[k+1,s],(scor_func$scoress[score_row,score_col]+M[k,s_dash]))
      }
    }
  }
colnames(M)<-initsa$states
  w<-Inf

  for(s in 1:l){
    w<-min(w[s],M[n+1,s])
  }
  return(M)
}
