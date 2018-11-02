#'
#'
#'
#'
#'
#'
#'
#'
BaumWelch<-function(initsa,x,y,m,error,theta=NULL){

  n<-length(x)
  l<-length(initsa$states)
  if(is.null(theta)){
    scor_func<-scores(initsa)
  }else{
    scor_func<-scores(NULL,theta)
  }
  theta<-scor_func$theta
  M<-matrix(data = NA, nrow = n+1,ncol = l)
  B<-matrix(data = NA, nrow = n+1,ncol = l)
  continue<-TRUE
  while (continue)

  {
    #  Calculating Forward & Backward Probabilities
    for(s in 1:l){
      M[1,s]<-1/(l*length(initsa$inputSymbols))
      B[n+1,s]<-1
    }
    j<-n

    for(k in 1:n){

      for(s in 1:l){
        M[k+1,s]<-0
        B[j,s]<-0
        for(s_dash in 1:l){
          #   score_col<-paste(toString(y[k]),toString(initsa$states[s]), sep=',')
          #   score_row<-paste(toString(x[k]),toString(initsa$states[s_dash]), sep=',')
          M[k+1,s]<-round(M[k+1,s]+(theta[paste(toString(x[k]),toString(initsa$states[s_dash]), sep=','),paste(toString(y[k]),toString(initsa$states[s]), sep=',')]*M[k,s_dash]),2)


          #  score_col<-paste(toString(y[j]),toString(initsa$states[s]), sep=',')
          #  score_row<-paste(toString(x[j]),toString(initsa$states[s_dash]), sep=',')
          B[j,s]=round(B[j,s]+(theta[paste(toString(x[j]),toString(initsa$states[s_dash]), sep=','),paste(toString(y[j]),toString(initsa$states[s]), sep=',')]*B[j+1,s_dash]),2)
        }
      }
      j<-j-1
    }
    # Number of time sequence (x,y) is observed in data set
    #  u<-TOC.sampleDatav2(initsa,x,y,m)
    u<-TOC.sampleData(initsa,m)

    u<-u/sum(M[n+1,])

# calculating v matrix.
    temp<-0
    vtemp<-theta
    vtemp[!is.na(vtemp)]<-0
    # print(v)
    colnames(M)<-initsa$states
    for (i in 1:n) {
      for (j in 1:n) {


        for (s in 1:l) {
          for (s_dash in 1:l) {

            for (kk in 1:n+1)

  {
              score_col<-paste(toString(y[j]),toString(initsa$states[s]), sep=',')
              score_row<-paste(toString(x[i]),toString(initsa$states[s_dash]), sep=',')


              temp<- temp+(M[kk-1,s]*theta[score_row,score_col]*B[kk,s_dash])

            }

            coln<-paste(toString(y[j]),toString(x[i]), sep=',')

            score_col<-paste(toString(y[j]),toString(initsa$states[s]), sep=',')
            score_row<-paste(toString(x[i]),toString(initsa$states[s_dash]), sep=',')
            vtemp[score_row,score_col]<-round((temp*u[coln])+vtemp[score_row,score_col],2)
          }

        }

      }
    }

    #  Calulating theta_hat (conditional Probabilities)
    #
    theta_hat<-theta
    theta_hat[!is.na(theta_hat)]<-0

    for (i in 1:length(initsa$inputSymbols)) {
      for (j in 1:length(initsa$outputSymbols)) {
        for (s in 1:l) {
          for (s_dash in 1:l) {
            score_col<-paste(toString(initsa$outputSymbols[j]),toString(initsa$states[s]), sep=',')
            score_row<-paste(toString(initsa$inputSymbols[i]),toString(initsa$states[s_dash]), sep=',')
            theta_hat[score_row,score_col]<-round(vtemp[score_row,score_col]/sum(vtemp[score_row,]),2)
          }
        }
      }
    }


    # Calculating Log-Likelihood Maximum Estimation for theta and theta_hat
    ltemp<-0
    l_hattemp<-0
    for (i in 1:length(initsa$inputSymbols)) {
      for (j in 1:length(initsa$outputSymbols)) {
        for (s in 1:l) {
          for (s_dash in 1:l) {
            score_col<-paste(toString(initsa$outputSymbols[j]),toString(initsa$states[s]), sep=',')
            score_row<-paste(toString(initsa$inputSymbols[i]),toString(initsa$states[s_dash]), sep=',')
            l_hattemp<-log(theta_hat[score_row,score_col])*vtemp[score_row,score_col]+l_hattemp
            ltemp<-log(theta[score_row,score_col])*vtemp[score_row,score_col]+ltemp
          }
        }
      }
    }

    l_hat<-l_hattemp
    ll<-ltemp
    if(l_hat-ll>error){
      theta<-theta_hat
    }
    else{
      continue<-FALSE
    }

  }
  theta_asterik<-theta_hat
#M<-Sforward(initsa,x,y,theta_asterik)
#B<-Sbackward(initsa,x,y,theta_asterik)

  return(theta_asterik)
}
