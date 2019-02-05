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
Version: \tab 2.3.0\cr
Date: \tab 2019-02-05\cr
License: \tab GPL version 3 or later\cr
}
% % The DESCRIPTION file:
% \packageDESCRIPTION{CMatching}
% \packageIndices{CMatching}
% ~~ An overview of how to use the package, ~~
% ~~ including the most important functions ~~
Several strategies have been suggested for adapting propensity score matching to clustered data. Depending on researcher's belief about the strength of unobserved cluster level covariates it is possible to take into account clustering either in the estimation of the propensity score model (through the inclusion of fixed or random effects, e.g. Arpino and Mealli (2011)) and/or in the implementation of the matching algorithm (see, e.g. Rickles and Seltzer (2014); Arpino and Cannas (2016)). 
This package contains main function \code{CMatch} to adapt classic matching algorithms for causal inference to clustered data and a customized \code{summary} function to analyze the output. 
Depending on the \code{type} argument function \code{CMatch} calls either \code{MatchW} implementing a \emph{pure} within-cluster matching or \code{MatchPW} implementing an approach which can be called \emph{"preferential"} within-cluster matching. This approach first looks for matchable units within the same cluster and - if no match is found - continues the search in the remaining clusters. The functions also provide causal estimands with cluster-adjusted standard errors from fitting a multilevel model on matched data. \code{CMatch} returns an object of class ''\code{CMatch}'' which can be be summarized and used as input of the \code{CMatchBalance} function to examine how much the procedure resulted in improved covariate balance. %Both functions are wrappers of the function \code{Match} but not all parameters are managed in the same way (\ eg. the replace=FALSE option is disabled in \code{MatchPW}) %In particular caliper is fixed with reference to \emph{overall} variation and model based standard errors are provided.A summary of the most important output can be obtained 
Although \code{CMatch} has been designed for dealing with clustered data, these algorithms can be used to force a perfect balance or to improve the balance of categorical variables, respectively. In this case, the "clusters" correspond to the levels of the categorical variable(s). When used for this purpouse the user should ignore the standard error (if provided). Note that \code{Matchby} from package \code{Matching} can be used for the same purpouse. 
}
\author{
\packageAuthor{CMatching}. A special thanks to Thomas W. Yee for his precious help in updating to version 2.1.

Maintainer: \packageMaintainer{CMatching}
}
\references{
%~~ Literature or other references for background information ~~
Sekhon, Jasjeet S. (2011). Multivariate and Propensity Score Matching Software with Automated Balance Optimization. \emph{Journal of Statistical Software, 42(7): 1-52.} http://www.jstatsoft.org/v42/i07/

Arpino, B., and Cannas, M. (2016). Propensity score matching with clustered data. An application to the estimation of the impact of caesarean section on the Apgar score. \emph{Statistics in Medicine, 35: 2074-2091}. doi: 10.1002/sim.6880.

Rickles, J. H., and Seltzer, M. (2014). A Two-Stage Propensity Score Matching Strategy for Treatment Effect Estimation in a Multisite Observational Study. \emph{Journal of Educational and Behavioral Statistics, 39(6), 612-636}. doi: 10.3102/1076998614559748

Arpino, B. and Mealli, F. (2011). The specification of the propensity score in multilevel observational studies. \emph{Computational Statistics & Data Analysis}, 55(4), 1770-1780. doi: 10.1016/j.csda.2010.11.008 
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

 \examples{
% ~~ simple examples of the most important functions ~~
# a paper and pencil example with a few units

id  <- c(1,2,3,4,5, 6,7,8,9,10)
 x  <- c( 1,1,1.1,1.1,1.4, 2,1,1,1.3, 1.3 )
 t  <- c( 1,1,1,1,0, 0,0,0,0, 0 )
 g  <- c(1,1,2,2,1,1,2,2,2, 2 ) # two groups of four and six units
toy <- t(data.frame(id,g, t,x))

# reorder units by ascending group
 toyord <-toy[,order(g)] 
 x <-toyord["x",]
 t <-toyord["t",]
 g <- toyord["g",]

# pooled matching
pm <- Match(Y=NULL, Tr=t, X=x, caliper=2,ties=FALSE,replace=FALSE)
# within matching 
wm <- CMatch(type="within",Y=NULL, Tr=t, X=x, Group=g,caliper=2,ties=FALSE,replace=FALSE)
# preferential-within matching
pwm <- CMatch(type="pwithin",Y=NULL, Tr=t, X=x, Group=g, caliper=2,ties=FALSE,replace=FALSE)
 
# quick look at matched dataset (matched pairs are vertically aligned)
# pooled
pm$index.treated
pm$index.control
# within
wm$index.treated
wm$index.control
# pref within
pwm$index.treated
pwm$index.control
 }
