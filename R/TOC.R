

#' @title  Learning (Not For End User)
#' @param initsa  SA Model.
#' @param  n  Length of input sample set sequence.
#' @usage  TOC.sampleData(initsa=NULL,n)
#' @description This function is not for end user.
#' @examples
#' \dontrun{
#' states<-c('s1','s2')
#' inputSymbols<-c('a','b')
#' outputSymbols<-c(0,1)
#' transProb<-matrix(c(0.70,0.50, 0.30,0.50), nrow = 2, ncol = 2,byrow = TRUE)
#' emissionProb<-matrix(c(0.50,0.30, 0.40,0.60,.50,.70,.60,.40), nrow = 2, ncol = 4, byrow = TRUE)
#' initsa<-initsa(states,inputSymbols,outputSymbols,emissionProb,transProb)
#' n<-3
#' TOC.sampleData(initsa, n)
#'
#'
#'
#' }





TOC.sampleData<-function(initsa=NULL,n){
  scor<-scores(initsa)
  #print(scor)
  s<-initsa$states
  for (ss in 1:n+1) {
    s<-outer(s,initsa$states, paste, sep=",")
    dim(s)<-NULL

  }

  n<-n-1
  inputs<-initsa$inputSymbols
  for (i in 1:n) {
    inputs<-outer(inputs,initsa$inputSymbols, paste, sep=",")
    dim(inputs)<-NULL

  }
  # print(inputs)

  outputs<-initsa$outputSymbols
  for (o in 1:n) {
    outputs<-outer(outputs,initsa$outputSymbols, paste, sep=",")
    dim(outputs)<-NULL

  }
  # print(outputs)

  mcnames<-outer(inputs,s, paste, sep=",")
  dim(mcnames)<-NULL
  mcnames<-outer(mcnames,outputs, paste, sep=",")
  dim(mcnames)<-NULL
  mrname<-outer(initsa$states, initsa$states, paste, sep=",")
  dim(mrname)<-NULL
  mrname<-outer(initsa$inputSymbols, mrname, paste, sep=",")
  dim(mrname)<-NULL
  mrname<-outer(mrname, initsa$outputSymbols, paste, sep=",")
  dim(mrname)<-NULL
  M<-matrix(data = 0, nrow =length(mrname), ncol = length(mcnames))
  colnames(M)<-mcnames
  A<-vector( mode = "numeric", length=length(mrname))
  #print(length(mcnames))
  rownames(M)<-mrname
  L<-M
  NameToVector<-is.vector(NULL)

  names(A)<-mrname
  for (i in 1:length(initsa$inputSymbols)) {
    for (j in 1:length(initsa$outputSymbols)) {

      for (k in 1:length(initsa$states)) {

        for (l in 1:length(initsa$states)) {
          # v<-NULL
          # v<-c(initsa$inputSymbols[i],initsa$inputSymbols[j])
          # v2<-c(initsa$states[k],initsa$states[l])
          # finds<-paste("(?=",initsa$inputSymbols[i],initsa$states[k],initsa$states[l],initsa$outputSymbols[j],")",sep="")
          # result<-lengths(regmatches(colnames(M), gregexpr(finds, colnames(M), perl = TRUE)))
          sc<-paste(initsa$inputSymbols[i],initsa$states[k],sep = ",")
          sr<-paste(initsa$outputSymbols[j],initsa$states[l],sep = ",")

          Arname<-paste(initsa$inputSymbols[i],initsa$states[k],initsa$states[l],initsa$outputSymbols[j],sep = ",")
          for (CN in 1:length(mcnames)) {
            occure<-0
            NameToVector<-strsplit(mcnames[CN], ",")[[1]]
            for (o in 1:n) {

              revNameToVector<-rev.default(NameToVector)

              for (r in 1:n) {
                s_dash<-strsplit(mcnames[CN], ",")[[1]]
                for (p in n+2:length(s_dash)-n) {
                  CombVector<-paste(NameToVector[o],s_dash[p-1],s_dash[p],revNameToVector[r],sep = ",")
                  Arname_dash<-strsplit(Arname, ",")[[1]]
                  if(Arname == CombVector && NameToVector[o]== Arname_dash[1] && Arname_dash[length(Arname_dash)]== revNameToVector[r]){

                    occure<-occure+1
                  }

                }

              }
            }

            # print(occure)
            M[Arname,CN]<-occure
          }


          # Arname<-paste(initsa$inputSymbols[i],initsa$states[k],initsa$states[l],initsa$outputSymbols[j],sep = "")
          A[Arname]<-sum(M[Arname,])
          #L[Arname,]<-round(M[Arname,]/sum(result),2)
        }
      }
    }

  }

  # print(NameToVector)
  # vecname<-names(A)
  # vecname<-gsub(",", "", vecname)
  # #vecname<-gsub("[0-9]+", "", vecname)
  # Atemp<-A
  # names(A)<-vecname


  un <-     rep(0,
                length(initsa$inputSymbols) * length(initsa$outputSymbols))
  uc <- 1

  for (i in 1:length(initsa$inputSymbols)) {
    for (j in 1:length(initsa$outputSymbols)) {
      for (s in 1:length(initsa$states)) {
        for (s_dash in 1:length(initsa$states)) {
          coln<-paste(toString(initsa$outputSymbols[j]),toString(initsa$inputSymbols[i]), sep=',')

          # paste(toString(row),toString(col),sep = ",")

          un[uc] <- coln
        }

      }

      uc <- uc + 1
    }

  }

  u <-     rep(0,
               length(initsa$inputSymbols) * length(initsa$outputSymbols))
  names(u) <-  un


  for (i in 1:length(initsa$inputSymbols)) {
    for (j in 1:length(initsa$outputSymbols)) {
      for (s in 1:length(initsa$states)) {
        for (s_dash in 1:length(initsa$states)) {

          coln<-paste(toString(initsa$outputSymbols[j]),toString(initsa$inputSymbols[i]), sep=',')

          indexName <-
            paste(
              toString(initsa$inputSymbols[i]),
              toString(initsa$states[s_dash]),
              toString(initsa$states[s]),
              toString(initsa$outputSymbols[j]),
              sep = ','
            )

          u[coln] <- u[coln] + A[indexName]
        }

      }


    }
    #
  }

  return(u)

}
