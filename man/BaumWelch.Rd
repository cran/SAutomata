\name{BaumWelch}
\alias{BaumWelch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Inferring the Forward and Backward Probabilities of a Stochastic Automata Model via the Baum-Welch algorithm
}
\description{
For an initial Stochastic Automata Model (SA) and a given sequence of observations, the Baum-Welch algorithm infers optimal forward and backward probabilities to the SA. Since the Baum-Welch algorithm is a variant of the Expectation-Maximisation algorithm, the algorithm converges to a local solution which might not be the global optimum.
}
\usage{
BaumWelch(initsa, x, y, m, error, theta = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{initsa}{
A Stochastic Automata Model.
}
  \item{x}{
A sequence of inputs.
}
  \item{y}{
A sequence of outputs.
}
  \item{m}{
Maximum length of sequence to create sample set for learning.
}
  \item{error}{
Maximum error rate.
}
  \item{theta}{
Optional Conditional Probabilities.
}
}

\value{
 Returns the conditional probabilities by learning the sample set.
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
m<-1
error<-10
BaumWelch(initsa, x, y, m, error)
}
