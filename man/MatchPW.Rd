\name{MatchPW}
\alias{MatchPW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Preferential Within-cluster Matching 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function implements "preferential" - within clusters matching. In other words, units that do not match within clusters (as defined by the \code{Group} variable) can match between cluster in the second step. 
}
\usage{
MatchPW(Y = NULL, Tr, X, Group = NULL, estimand = "ATT", M = 1,
 exact = NULL, caliper = 0.25, replace = TRUE, ties = TRUE, weights = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
%%     ~~Describe \code{Y} here~~
A vector containing the outcome of interest. 
}
  \item{Tr}{
%%     ~~Describe \code{Tr} here~~
A vector indicating the treated and control units. 
}
  \item{X}{
%%     ~~Describe \code{X} here~~
A matrix of covariates we wish to match on. This matrix should contain all confounders or the propensity score or a combination of both.
}
  \item{Group}{
%%     ~~Describe \code{Group} here~~
A vector describing the clustering structure (typically the cluster ID). This can be any numeric vector of the same length of \code{Tr} and \code{X}  containing integer numbers in ascending order otherwise an error message will be returned. Default is NULL, however if \code{Group} is missing or contains only one integer the output of the \emph{Match} function is returned with a warning.  
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
%%     ~~Describe \code{caliper} here~~
A maximum allowed distance for matching units. Units for which no match was found within caliper distance are discarded. Default is 0.25. The caliper is interpreted in standard deviation units of the \emph{unclustered} data for each variable. For example, if caliper=0.25 all matches at distance bigger than 0.25 times the standard deviation for any of the variables in \code{X} are discarded. The caliper is used for both within and between clusters matching.
}
  \item{replace}{
%%     ~~Describe \code{replace} here~~
Default is TRUE. Note that setting the parameter to FALSE would give a warning since only the within-matching part can be performed without replacement (see Details).
}
  \item{ties}{
%%     ~~Describe \code{ties} here~~
An indicator for dealing with multiple matches. If more than M matches are found for each unit the additional matches are a) wholly retained with equal weights if ties=TRUE; b) a random one is chosen if ties=FALSE. Default is TRUE.
}
  \item{weights}{
%%     ~~Describe \code{weights} here~~
A vector of observation specific weights.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
Please note that all additional arguments of the \code{Match} function are not used.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The function performs preferential-within matching in the clusters defined by the variable \code{Group}. In the first phase matching within clusters is performed (see \code{MatchW}) and in the second the unmatched treated (or controls if estimand="ATC") are matched with all controls (treated) units. This can be helpful to avoid dropping many units in small clusters.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
  \item{index.control}{The index of control observations in the matched dataset.}
  \item{index.treated}{The index of control observations in the matched dataset.}
    \item{index.dropped}{The index of dropped observations due to the exact or caliper option. Note that these observations are treated if estimand is "ATT", controls if "ATC".}
     \item{est}{The causal estimate. This is provided only if \code{Y} is not null. If estimand is "ATT" it is the (weighted) mean of \code{Y} in matched treated minus the (weighted) mean of \code{Y} in matched controls. Equivalently it is the weighted average of the within-cluster ATT's', with weights given by cluster sizes.} 
      \item{se}{A model-based standard error for the causal estimand. This is a cluster robust estimator of the standard error for the linear model: \code{y ~ constant+Tr}, run on the matched dataset (see \code{\link{cluster.vcov}} for details on how this estimator is obtained).}
\item{mdata}{The matched datasets. These datasets can also be recovered using index.treated and index.controls.}
\item{orig.treated.nobs.by.group}{The original number of treated observations by group in the dataset.}
\item{orig.control.nobs.by.group}{The original number of control observations by group in the dataset.}
\item{orig.dropped.nobs.by.group}{The number of dropped observations by group after within cluster matching.}
\item{orig.dropped.nobs.by.group.after.prefwithin}{The number of dropped observations by group after preferential within group matching.}
\item{orig.nobs}{The original number of observations in the dataset.}
\item{orig.wnobs}{The original number of weighted observations in the dataset.}
\item{orig.treated.nobs}{The original number of treated observations in the dataset.}
\item{orig.control.nobs}{The original number of control observations in the dataset.}
\item{wnobs}{the number of weighted observations in the matched dataset.}
\item{caliper}{The caliper used.}
\item{intcaliper}{The internal caliper used.}
\item{exact}{The value of the exact argument.}
\item{ndrops.matches}{The number of matches dropped either because of the caliper or exact option.}
\item{estimand}{The estimand required.}
}
\references{
%% ~put references to the literature/web site here ~
Sekhon, Jasjeet S. 2011. Multivariate and Propensity Score Matching Software with Automated Balance Optimization. \emph{Journal of Statistical Software 42(7): 1-52.} http://www.jstatsoft.org/v42/i07/

Arpino, B., and Cannas, M. (2016) Propensity score matching with clustered data. An application to the estimation of the impact of caesarean section on the Apgar score. \emph{Statist. Med., 35: 2074–2091}. doi: 10.1002/sim.6880.
}
\author{
%%  ~~who you are~~
\packageAuthor{CMatching}
}
\note{
%%  ~~further notes~~
The function returns an object of class \code{Match}. This allows compatibility with the \code{MatchBalance} function which can be used to examine the covariate balance before and after matching. See the examples below.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~\emph{

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
See also \code{\link{Match}}, \code{\link{MatchBalance}}
}
\examples{
data(schools)
 
# Kreft and De Leeuw, Introducing Multilevel Modeling, Sage (1988).   
# The data set is the subsample of NELS-88 data consisting
# of 10 handpicked schools from the 1003 schools in the full data set.

# Let us consider the following variables:

X<-schools$ses # X may contain more covariates (all confounders)
Y<-schools$math
Tr<-ifelse(schools$homework>1,1,0)
Group<-schools$schid

# Let us assume that the effect of homeworks (Tr) on math score (Y)
# is unconfounded conditional on X and other unobserved schools features.
# Several strategies to handle unobserved group characteristics
# are described in Arpino & Cannas, 2016 (see References).

#### Multivariate Matching on covariates in X (default parameters:
# one-to-one matching on X with replacement with a caliper of 0.25).

# Match within schools
 mw<-MatchW(Y=schools$math, Tr=Tr, X=schools$ses, Group=schools$schid, caliper=0.1)
 summary(mw)
 
# Match preferentially within school (first match within schools
# and then (try to) match remaining units between schools)
mpw <- MatchPW(Y=schools$math, Tr=Tr, X=schools$ses, Group=schools$schid, caliper=0.1)
summary(mpw)

# compare balance before and after matching
bmw  <- MatchBalance(Tr~ses,data=schools,match.out=mw)
bmpw<- MatchBalance(Tr~ses,data=schools,match.out=mpw)

 # see drops by school
 mpw$orig.dropped.nobs.by.group.after.within       # after within
 mpw$orig.dropped.nobs.by.group.after.prefwithin # after preferential within

#### Propensity score matching

# estimate the ps model

mod <- glm(Tr~ses+parented+public+sex+race+urban,
family=binomial(link="logit"),data=schools)
eps <- fitted(mod)

# propensity score matching within schools

psmw <- MatchW(Y=schools$math, Tr=Tr, X=eps, Group=schools$schid, caliper=0.1)

# can combine MatchW and MatchPW with several specifications of ps:
# eg 1: match within using ps estimated from dummy logit model 

mod <- glm(Tr ~ ses + parented + public + sex + race + urban
 +schid - 1,family=binomial(link="logit"),data=schools)
eps <- fitted(mod)

dpsm <- MatchW(Y=schools$math, Tr=Tr, X=eps, Group=schools$schid, caliper=0.1)

# eg2: classic matching using ps estimated from multilevel logit model 

require(lme4)
mod<-glmer(Tr ~ ses + parented + public + sex + race + urban + (1|schid),
family=binomial(link="logit"), data=schools)
eps <- fitted(mod)

mpsm<-MatchW(Y=schools$math, Tr=Tr, X=eps, Group=NULL, caliper=0.1)
# or simply run Match with X=eps
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ matching }
\keyword{ clustered data }% __ONLY ONE__ keyword per line
