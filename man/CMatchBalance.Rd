\name{CMatchBalance}
\alias{CMatchBalance}
\alias{CMatchBalance.default}
\alias{CMatchBalance.CMatch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Analyze covariate balance before and after matching.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Generic function for analyzing covariate balance. If \code{match.out} is \code{NULL} only balance statistics for the unmatched data are returned otherwise both before and after matching balance are given. The function is simply a wrapper calling \code{MatchBalance}, possibly after coercing the class of \code{match.out}. See \code{MatchBalance} for more detailed description.
}
\usage{
CMatchBalance(match.out, formula, data = NULL, ks = TRUE, 
nboots = 500, weights = NULL, digits = 5, paired = TRUE, print.level = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{match.out}{
%%     ~~Describe \code{match.out} here~~
A matched data set, i.e., the result of a call to \code{Match} or \code{CMatch}. 
}
  \item{formula}{
%%     ~~Describe \code{match.out} here~~
This formula does not estimate a model. It is a compact way to describe which variables should be compared between the treated and control group. See \code{MatchBalance}. 
}
  \item{data}{
%%     ~~Describe \code{match.out} here~~
An optional data set for the variables indicated in the \code{formula} argument.
}
  \item{ks}{
%%     ~~Describe \code{match.out} here~~
A flag for whether Kolmogorov-Smirnov tests should be calculated.
}
  \item{weights}{
%%     ~~Describe \code{match.out} here~~
A vector of observation-specific weights.
}
  \item{nboots}{
%%     ~~Describe \code{match.out} here~~
The number of bootstrap replication to be used.
}
  \item{digits}{
%%     ~~Describe \code{match.out} here~~
The number of digits to be displayed in the output
}
  \item{paired}{
%%     ~~Describe \code{match.out} here~~
A flag for whether a paired t.test should be used for the matched data. An unpaired t.test is always used for unmatched data.
}
  \item{print.level}{
%%     ~~Describe \code{match.out} here~~
The amount of printing, taking values 0 (no printing), 1(summary) and 2 (dtailed results). Default to 1.
}
  % \item{\dots}{
  	% Other arguments to be passed to the \code{MatchBalance} function.
% %%     ~~Describe \code{\dots} here~~
%}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The function is a wrapper of the \code{MatchBalance} function. If \code{match.out} is of class \code{Match} (or \code{NULL}) then it calls \code{MatchBalance}. If \code{match.out} is of class\code{CMatch} then it coerces the class to \code{Match} before calling \code{MatchBalance}. This function is meant to exploit \code{MatchBalance} for \code{CMatch} objects for which \code{MatchBalance} would not work. 
}
\value{
	Balance statistics for the covariates specified in the \emph{right} side of \code{formula} argument. Statistics are compared between the two groups specified by the binary variable in the \emph{left} side of \code{formula}.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
Sekhon, Jasjeet S. 2011. Multivariate and Propensity Score Matching Software with Automated Balance Optimization. \emph{Journal of Statistical Software 42(7): 1-52.} http://www.jstatsoft.org/v42/i07/
}
\author{
%%  ~~who you are~~
\packageMaintainer{CMatching} and a special thanks to Thomas W. Yee for his precious help.
}
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{MatchBalance}}
}
\examples{

data(schools)
 
# Kreft and De Leeuw, Introducing Multilevel Modeling, Sage (1988).   
# The data set is the subsample of NELS-88 data consisting of 10 handpicked schools
# from the 1003 schools in the full data set.
 
# Suppose that the effect of homeworks on math score is unconfounded conditional on X 
# and unobserved school features (we assume this only for illustrative purpouse).

# Let us consider the following variables:

X<-schools$ses # or X<-as.matrix(schools[,c("ses","white","public")])
Y<-schools$math
Tr<-ifelse(schools$homework>1,1,0)
Group<-schools$schid
# When Group is missing or there is only one Group CMatch returns 
# the output of the Match function with a warning.

# Let us assume that the effect of homeworks (Tr) on math score (Y)
# is unconfounded conditional on X and other unobserved school features.
# Several strategies to handle unobserved group characteristics
# are described in Arpino & Cannas, 2016 (see References). 


# Multivariate Matching on covariates in X 
# default parameters: one-to-one matching on X with replacement with a caliper of 0.25.

### Matching within schools
 mw<-CMatch(type="within",Y=Y, Tr=Tr, X=X, Group=Group, caliper=0.1)
 
 # compare balance before and after matching
 bmw  <- CMatchBalance(Tr~X,data=schools,match.out=mw)
 
 # calculate proportion of matched observations
  (mw$orig.treated.nobs-mw$ndrops)/mw$orig.treated.nobs 
  
 # check number of drops by school
 mw$orig.ndrops.by.group 
 
 
### Match preferentially within school 
# i.e. first match within schools
# then (try to) match remaining units between schools
 mpw <- CMatch(type="pwithin",Y=schools$math, Tr=Tr, X=schools$ses, 
 Group=schools$schid, caliper=0.1)

# examine covariate balance
  bmpw<- CMatchBalance(Tr~ses,data=schools,match.out=mpw)
  # equivalent to MatchBalance(...) with mpw coerced to class "Match"

# proportion of matched observations
  (mpw$orig.treated.nobs-mpw$ndrops) / mpw$orig.treated.nobs 
# check drops by school
  mpw$orig.ndrops.by.group  
# proportion of matched observations after match-within only
 (mpw$orig.treated.nobs-sum(mpw$orig.ndrops.by.group.after.within)) / mpw$orig.treated.nobs
 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ matching }
\keyword{ covariate balance }
