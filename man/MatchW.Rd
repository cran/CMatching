\name{MatchW}
\alias{MatchW}
\title{
Within-cluster Matching 
}
\description{
This function implements multivariate and propensity score matching within clusters defined by the \code{Group} variable.   
}
\usage{
MatchW(Y = NULL, Tr, X, Group = NULL, estimand = "ATT", M = 1, 
exact = NULL, caliper = 0.25, weights = NULL, replace = TRUE, ties = TRUE, ...)
}
\arguments{
  \item{Y}{
A vector containing the outcome of interest. 
}
  \item{Tr}{
A vector indicating the treated and control units. 
}
  \item{X}{
A matrix of covariates we wish to match on. This matrix should contain all confounders or the propensity score or a combination of both.
}
  \item{Group}{
%%     ~~Describe \code{Group} here~~
A vector describing the clustering structure (typically the cluster ID). This can be any numeric vector of the same length of \code{Tr} and \code{X}  containing integer numbers in ascending order otherwise an error message will be returned. Default is NULL, however if \code{Group} is missing, NULL or it contains only one value the output of the \code{Match} function is returned with a warning.   
}
  \item{estimand}{
%%     ~~Describe \code{estimand} here~~
The causal estimand desired, one of "ATE", "ATT" and "ATC", which stand for Average Treatment Effect, Average Treatment effect on the Treated and on the Controls, respectively. Default is "ATT". 
}
  \item{M}{
%%     ~~Describe \code{M} here~~
The number of matches which are sought for each unit. Default is 1 ("one-to-one matching").
}
  \item{exact}{
%%     ~~Describe \code{exact} here~~
An indicator for whether exact matching on the variables contained in \code{X} is desired. Default is FALSE. This option has precedence over the caliper option.
}
  \item{caliper}{
A maximum allowed distance for matching units. Units for which no match was found within caliper distance are discarded. Default is 0.25. The caliper is interpreted in standard deviation units of the \emph{unclustered} data for each variable. For example, if caliper=0.25 all matches at distance bigger than 0.25 times the standard deviation for any of the variables in \code{X} are discarded. 
}
  \item{weights}{
A vector of specific observation weights.
}
  \item{replace}{
Matching can be with or without replacement depending on whether matches can be re-used or not. Default is TRUE.
}
  \item{ties}{
An indicator for dealing with multiple matches. If more than M matches are found for each unit the additional matches are a) wholly retained with equal weights if ties=TRUE; b) a random one is chosen if ties=FALSE. Default is TRUE.
}
  \item{\dots}{
Note that additional arguments of the Match function are not used.
}
}
\details{
This function is meant to be a natural extension of the \code{Match} function to clustered data. It retains the main arguments of \code{Match} but it has additional output showing matching results cluster by cluster.
 It differs from wrapper \code{Matchby} in package \code{Matching} in the way standard errors are calculated and because the caliper is in standard deviation units of the covariates on the overall dataset (so the caliper is the same for all clusters). Moreover, observation weights are available. 
}
\value{
  \item{index.control}{The index of control observations in the matched dataset.}
  \item{index.treated}{The index of control observations in the matched dataset.}
    \item{index.dropped}{The index of dropped observations due to the exact or caliper option. Note that these observations are treated if estimand is "ATT", controls if "ATC".}
     \item{est}{The causal estimate. This is provided only if \code{Y} is not null. If estimand is "ATT" it is the (weighted) mean of \code{Y} in matched treated units minus the (weighted) mean of \code{Y} in matched controls. Equivalently, it is the weighted average of the within-cluster ATTs, with weights given by cluster sizes in the matched dataset.} 
      \item{se}{A model-based standard error for the causal estimand. This is a cluster robust estimator of the standard error for the linear model: \code{Y ~ constant+Tr}, run on the matched dataset (see \code{\link[multiwayvcov]{cluster.vcov}} for details on how this estimator is obtained). Note that these standard errors differ from a weighted average of cluster specific standard errors provided by the \code{Matchby} function, which are generally larger. Estimating standard errors for causal parameters with clustered data is an active field of research and there is no perfect solution to date.}   
\item{mdata}{A list containing the matched datasets produced by \code{MatchPW}. Three datasets are included in this list: \code{Y}, \code{Tr} and \code{X}. The matched dataset for \code{Group} can be recovered by \code{rbind(Group[index.treated],Group[index.control])}.}

\item{orig.treated.nobs.by.group}{The original number of treated observations by group in the dataset.}

\item{orig.control.nobs.by.group}{The original number of control observations by group in the dataset.}

\item{orig.dropped.nobs.by.group}{The number of dropped observations by group after within cluster matching.}
\item{orig.nobs}{The original number of observations in the dataset.}
\item{orig.wnobs}{The original number of weighted observations in the dataset.}
\item{orig.treated.nobs}{The original number of treated observations in the dataset.}
\item{orig.control.nobs}{The original number of control observations in the dataset.}
\item{wnobs}{the number of weighted observations in the matched dataset.}
\item{caliper}{The caliper used.}
\item{intcaliper}{The internal caliper used.}
\item{exact}{The value of the exact argument.}
\item{ndrops.matches}{The number of matches dropped either because of the caliper or exact option (or because of forcing the match within-clusters).}
\item{estimand}{The estimand required.}
}
\references{

Sekhon, Jasjeet S. 2011. Multivariate and Propensity Score Matching Software with Automated Balance Optimization. \emph{Journal of Statistical Software 42(7): 1-52.} http://www.jstatsoft.org/v42/i07/

Arpino, B., and Cannas, M. (2016) Propensity score matching with clustered data. An application to the estimation of the impact of caesarean section on the Apgar score. \emph{Statistics in Medicine, 35: 2074–2091}. doi: 10.1002/sim.6880.

}
\author{

\packageMaintainer{CMatching}
}
\note{

The function returns an object of class \code{CMatch}. The \code{CMatchBalance} function can be used to examine the covariate balance before and after matching (see the examples below).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also See also \code{\link[Matching]{Match}}, \code{ \link[Matching]{MatchBalance}},\code{\link[multiwayvcov]{cluster.vcov}}
}

\examples{
data(schools)
	
# Kreft and De Leeuw, Introducing Multilevel Modeling, Sage (1988).   
# The data set is the subsample of NELS-88 data consisting of 10 handpicked schools 
# from the 1003 schools in the full data set.
 
# Let us consider the following variables:

X<-schools$ses 
Y<-schools$math
Tr<-ifelse(schools$homework>1,1,0)
Group<-schools$schid

# Note that when Group is missing / NULL or there is only one group the function MatchW returns
# the output of the Match function with a warning.

# Matching math scores between gropus of students. X are covariate(s) we wish to match on. 

### Matching within schools
 mw <- MatchW(Y=Y, Tr=Tr, X=X, Group=Group, caliper=0.1)
 
 # compare balance before and after matching
   CMatchBalance(Tr~X,data=schools,match.out=mw)
 
 # find proportion of matched observations
  (mw$orig.treated.nobs-mw$ndrops)/mw$orig.treated.nobs 
  
 # check number of drops by school
 mw$orig.ndrops.by.group 
 
 # estimate the math score difference (default is ATT)
  mw$estimand
 
 # examine output
 mw                   # complete results                 
 summary(mw)          # main results
 
 
#### Propensity score matching

# estimate the propensity score (ps) model

mod <- glm(Tr~ses+parented+public+sex+race+urban,
family=binomial(link="logit"),data=schools)
eps <- fitted(mod)

# eg 1: within-school propensity score matching
psmw <- MatchW(Y=schools$math, Tr=Tr, X=eps, Group=schools$schid, caliper=0.1)

# We can use other strategies for controlling unobserved cluster covariates
# by using different specifications of ps:

# eg 2: standard propensity score matching using ps estimated
# from a logit model with dummies for schools

mod <- glm(Tr ~ ses + parented + public + sex + race + urban 
+schid - 1,family=binomial(link="logit"),data=schools)
eps <- fitted(mod)



dpsm <- MatchW(Y=schools$math, Tr=Tr, X=eps, caliper=0.1)
# this is equivalent to run Match with X=eps

# eg3: standard propensity score matching using ps estimated from 
# multilevel logit model (random intercept at the school level)

require(lme4)
mod<-glmer(Tr ~ ses + parented + public + sex + race + urban + (1|schid),
family=binomial(link="logit"), data=schools)
eps <- fitted(mod)

mpsm<-MatchW(Y=schools$math, Tr=Tr, X=eps, Group=NULL, caliper=0.1)
# this is equivalent to run Match with X=eps
 

}
\keyword{ matching }
\keyword{ clustered data }
