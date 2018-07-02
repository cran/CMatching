\name{CMatching-package}
\alias{CMatching-package}
\alias{CMatching}
\docType{package}
\title{
\packageTitle{CMatching}
}
\description{
\packageDescription{CMatching}
}
\details{
	\tabular{ll}{
Package: \tab CMatching\cr
Type: \tab Package\cr
Version: \tab 2.1\cr
Date: \tab 2018-07-01\cr
License: \tab GPL version 3 or later\cr
}
% % The DESCRIPTION file:
% \packageDESCRIPTION{CMatching}
% \packageIndices{CMatching}
% ~~ An overview of how to use the package, ~~
% ~~ including the most important functions ~~
  Arpino and Cannas (2016) described several strategies to handle unobserved cluster characteristics in causal inference estimation with clustered data. Depending on researcher's belief about the strength of unobserved cluster level covariates it is possible to take into account clustering either in the estimation of the propensity score model (through the inclusion of fixed or random effects) and/or in the implementation of the matching algorithm. 
The package contains function \code{CMatch} to adapt classic matching algorithms for causal inference to clustered data and a customized \code{summary} function to analyze the output. 
Depending on the \code{type} argument function \code{CMatch} either calls either \code{MatchW} implementing a \emph{pure} within-cluster matching or function \code{MatchPW} implementing an approach which can be called \emph{"preferential"} within-cluster matching. The preferential approach first searches for matchable units within the same cluster. If no match was found the algorithm searches in other clusters. The functions also provide causal estimands with cluster-adjusted standard errors from fitting a multilevel model on matched data. \code{CMatch} returns an object of class ''\code{CMatch}'' which can be be summarized and used as input of the \code{CMatchBalance} function to examine how much the procedure resulted in improved covariate balance. %Both functions are wrappers of the function \code{Match} but not all parameters are managed in the same way (\ eg. the replace=FALSE option is disabled in \code{MatchPW}) %In particular caliper is fixed with reference to \emph{overall} variation and model based standard errors are provided.A summary of the most important output can be obtained 
Although \code{CMatch} has been designed for dealing with clustered data, these algorithms can be used to force a perfect balance or to improve the balance of categorical variables, respectively. In this case, the “clusters” correspond to the levels of the categorical variable(s). When used for this purpouse the user should ignore the standard error (if provided). Note that \code{Matchby} from package \code{Matching} can be used for the same purpouse. 
}
\author{
\packageAuthor{CMatching} and a special thanks to Thomas W. Yee for his precious help.

Maintainer: \packageMaintainer{CMatching}
}
\references{
%~~ Literature or other references for background information ~~
Sekhon, Jasjeet S. 2011. Multivariate and Propensity Score Matching Software with Automated Balance Optimization. \emph{Journal of Statistical Software 42(7): 1-52.} http://www.jstatsoft.org/v42/i07/

Arpino, B., and Cannas, M. (2016) Propensity score matching with clustered data. An application to the estimation of the impact of caesarean section on the Apgar score. \emph{Statistics in Medicine, 35: 2074–2091}. doi: 10.1002/sim.6880.
}
% ~~ Optionally other standard keywords, one per ~~
% ~~ line, from file KEYWORDS in the R ~~
% ~~ documentation directory ~~
\keyword{causal inference }
\keyword{cluster}

\seealso{
%~~ Optional links to other man pages, e.g. ~~
\code{\link{Match}},\code{ \link{MatchBalance}}
}

% \examples{
% ~~ simple examples of the most important functions ~~
% }
