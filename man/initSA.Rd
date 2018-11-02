\name{initSA}
\alias{initSA}
\title{
Initialisation of SA's
}
\description{
This function initialises a general discrete time and discrete space Stochastic Automata(SA). A SA consists of an alphabet of states, input and output symbols. The SA is designed to make inference on the states through the observation of input symbols on output symbols. The stochastics of the SA is fully described by the set of states, input and output symbols and the conditional probablities (i.e. state transition probablity and output symbols emission probablity by inputs symbols on state set).
}
\usage{
initSA(states,inputSymbols,outputSymbols,emissionProb,transitionProb)
}

\arguments{
  \item{states}{
        Vector with names of states.
}
  \item{inputSymbols}{
        Vector with names of input Symbols.
}
 \item{outputSymbols}{
        Vector with names of output Symbols.
}
 \item{emissionProb}{
       Stochastic matrix containing emission probablities of output symbols between states and input symbols.
}
 \item{transitionProb}{
       Stochastic matrix containing probablities between states.
}
}
\details{
    The column sum of \code{transitionProb} and \code{emissionProb} must be equal to 1. Otherwise this function generates an error message.
}
\value{
    This function \code{initSA} returns an SA that consists of a list of 5 elements:

 \item{States }{Vector with names of states.}
 \item{inputSymbols }{ Vector with names of input Symbols.}
 \item{outputSymbols }{ Vector with names of output Symbols.}
 \item{outputSymbols }{ Vector with names of output Symbols.}
 \item{emissionProb}{
       Annotated matrix containing emission probablities of output symbols between states and input symbols.
}
 \item{transitionProb}{
       Annotated matrix containing probablities between states.
}

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
}
