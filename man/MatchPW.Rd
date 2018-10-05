\name{MatchPW}
\alias{MatchPW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Preferential Within-cluster Matching 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function implements preferential within-cluster matching. In other words, units that do not match within clusters (as defined by the \code{Group} variable) can match between cluster in the second step. 
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
A vector describing the clustering structure (typically the cluster ID). This can be any numeric vector of the same length of \code{Tr} and \code{X}  containing integer numbers in ascending order otherwise an error message will be returned. Default is NULL, however if \code{Group} is missing, NULL or contains only one value the output of the \emph{Match} function is returned with a warning.  
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
The function performs preferential within-cluster matching in the clusters defined by the variable \code{Group}. In the first phase matching within clusters is performed (see \code{MatchW}) and in the second the unmatched treated (or controls if estimand="ATC") are matched with all controls (treated) units. This can be helpful to avoid dropping many units in small clusters.
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
     \item{est}{The causal estimate. This is provided only if \code{Y} is not null. If estimand is "ATT" it is the (weighted) mean of \code{Y} in matched treated minus the (weighted) mean of \code{Y} in matched controls. Equivalently it is the weighted average of the within-cluster ATTs, with weights given by cluster sizes in the matched dataset.} 
      \item{se}{A model-based standard error for the causal estimand. This is a cluster robust estimator of the standard error for the linear model: \code{y ~ constant+Tr}, run on the matched dataset (see \code{\link{cluster.vcov}} for details on how this estimator is obtained).}
\item{mdata}{A list containing the matched datasets produced by \code{MatchPW}. Three datasets are included in this list: \code{Y}, \code{Tr} and \code{X}. The matched dataset for \code{Group} can be recovered by \code{rbind(Group[index.treated],Group[index.control])}.
}
\item{orig.treated.nobs.by.group}{The original number of treated observations by group in the dataset.}
\item{orig.control.nobs.by.group}{The original number of control observations by group in the dataset.}
\item{orig.dropped.nobs.by.group}{The number of dropped observations by group after within cluster matching.}
\item{orig.dropped.nobs.by.group.after.pref.within}{The number of dropped observations by group after preferential within group matching.}
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

Arpino, B., and Cannas, M. (2016) Propensity score matching with clustered data. An application to the estimation of the impact of caesarean section on the Apgar score. \emph{Statistics in Medicine, 35: 2074–2091}. doi: 10.1002/sim.6880.
}
\author{
%%  ~~who you are~~
\packageMaintainer{CMatching}
}
\note{
%%  ~~further notes~~
The function returns an object of class \code{CMatch}. The \code{CMatchBalance} function can be used to examine the covariate balance before and after matching. See the examples below.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~\emph{

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
See also \code{\link{Match}},\code{ \link{MatchBalance}}
}
\examples{
data(schools)
	
# Kreft and De Leeuw, Introducing Multilevel Modeling, Sage (1988).   
# The data set is the subsample of NELS-88 data consisting of 10 handpicked schools 
# from the 1003 schools in the full data set.
 
# Let us consider the following variables:

X<-schools$ses #X<-as.matrix(schools[,c("ses","white","public")])
Y<-schools$math
Tr<-ifelse(schools$homework>1,1,0)
Group<-schools$schid
# Note that when Group is missing, NULL or there is only one Group the function
# returns the output of the Match function with a warning.

# Suppose that the effect of homeworks (Tr) on math score (Y)
# is unconfounded conditional on X and other unobserved schools features.
# Several strategies to handle unobserved group characteristics
# are described in Arpino and Cannas, 2016 (see References). 


# Multivariate Matching on covariates in X 
# default parameters: one-to-one matching on X
# with replacement with a caliper of 0.25; see also \code{Match}).
 

 
### Match preferentially within school 
# first match within schools
# then (try to) match remaining units between schools
 mpw <- MatchPW(Y=schools$math, Tr=Tr, X=schools$ses, Group=schools$schid, caliper=0.1)
 # equivalent to
 # CMatch(type="pwithin",Y=schools$math, Tr=Tr, X=schools$ses, 
 #   Group=schools$schid, caliper=0.1)

# examine covariate balance
  bmpw<- CMatchBalance(Tr~ses,data=schools,match.out=mpw)

# proportion of matched observations
  (mpw$orig.treated.nobs-mpw$ndrops) / mpw$orig.treated.nobs 
# check drops by school
  mpw$orig.ndrops.by.group  
# proportion of matched observations after match-within only
  (mpw$orig.treated.nobs-sum(mpw$orig.ndrops.by.group.after.within)) / mpw$orig.treated.nobs

# complete output
   mpw
# or use summary method for main results
   summary(mpw) 


#### Propensity score matching

# estimate the propensity score (ps) model

mod <- glm(Tr~ses+parented+public+sex+race+urban,
family=binomial(link="logit"),data=schools)
eps <- fitted(mod)

# eg 1: preferential within-school propensity score matching
psmw <- MatchPW(Y=schools$math, Tr=Tr, X=eps, Group=schools$schid, caliper=0.1)

# We can use other strategies for controlling unobserved cluster covariates
# by using different specifications of ps (see Arpino and Mealli for details):

# eg 2: standard propensity score matching using ps estimated
# from a logit model with dummies for schools

mod <- glm(Tr ~ ses + parented + public + sex + race + urban 
+schid - 1,family=binomial(link="logit"),data=schools)
eps <- fitted(mod)



dpsm <- MatchPW(Y=schools$math, Tr=Tr, X=eps, caliper=0.1)
# this is equivalent to run Match with X=eps

# eg3: standard propensity score matching using ps estimated from 
# multilevel logit model (random intercept at the school level)


require(lme4)
mod<-glmer(Tr ~ ses + parented + public + sex + race + urban + (1|schid),
family=binomial(link="logit"), data=schools)
eps <- fitted(mod)


mpsm<-MatchPW(Y=schools$math, Tr=Tr, X=eps, Group=NULL, caliper=0.1)
# this is equivalent to run Match with X=eps
 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ matching }
\keyword{ clustered data }% __ONLY ONE__ keyword per line

