\name{Sforward}
\alias{Sforward}

\title{
Computes The Forward Probabilities
}
\description{
The \code{Sforward} function computes the forward probabilities. The forward probabilities for state 'S' up to output observations at time k is defined as the probability of observing the sequance of observations 'Y'(y_1, ... ,y_k) and that state at time 'k' is 'S'. that is:\cr
f[k,X] := Prob(Y_1 = y_1, ... , Y_k = y_k ,S_k = S).\cr
Where Y_1, ... ,Y_n = y_1, ... , y_n is sequance of observed emissions and S_k is a random variable that represents the state at time k.
}
\usage{
Sforward(initsa, x, y, theta=NULL)
}

\arguments{
  \item{initsa}{
    A Stochastic Model.
}
  \item{x}{
 A vector of input sequance.
}
  \item{y}{
A vector of Output sequance.
}
  \item{theta}{
Optional Conditional Probabilities.
}
}

\value{
Return Value:\cr
\code{forward} A matrix containing the forward probabilities. The probabilities are given on a logarithmic scale (natural logarithm). This first dimension refer to the time and the second dimension to states.
}

\author{
Rehman Ahmad <rehman.ahmad777@gmail.com>
}

\examples{
states<-c('s1','s2')
inputSymbols<-c('a','b')
outputSymbols<-c(0,1)
transProb<-matrix(c(0.70,0.50, 0.30,0.50), nrow = 2, ncol = 2,byrow = TRUE)
emissionProb<-matrix(c(0.50,0.30, 0.40,0.60,.50,.70,.60,.40), nrow = 2, ncol = 4, byrow = TRUE)
initsa<-initSA(states,inputSymbols,outputSymbols,emissionProb,transProb)
x<-c('b','a')
y<-c(0,1)
sf<-Sforward(initsa, x, y)
}
