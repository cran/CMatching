\name{schools}
\alias{schools}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Schools data set (NELS-88)
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
Data set used by Kreft and De Leeuw in their book \emph{Introducing Multilevel Modeling, Sage (1988)} to analyse the relationship between math score and time spent by students to do math homework.
The data set is a subsample of NELS-88 data consisting of 10 handpicked schools from the 1003 schools in the full data set. Students are nested within schools and information is available both at the school and student level.
}
\usage{data("schools")}
\format{
  A data frame with 260 observations on the following 19 variables.
  \describe{
    \item{\code{schid}}{School ID: a numeric vector identyfing each school.}
    \item{\code{stuid}}{The student ID.}
    \item{\code{ses}}{Socioeconomic status.}
    \item{\code{meanses}}{Mean ses for the school.}
    \item{\code{homework}}{The number of hours spent weekly doing homeworks.}
    \item{\code{white}}{A dummy for white race (=1) versus non-white (=0).}
    \item{\code{parented}}{Parents highest education level.}
    \item{\code{public}}{Public school: 1=public, 0=non public.}
    \item{\code{ratio}}{Student-teacher ratio.}
    \item{\code{percmin}}{Percent minority in school.}
    \item{\code{math}}{Math score}
    \item{\code{sex}}{Sex: 1=male, 2=female.}
    \item{\code{race}}{Race of student, 1=asian, 2=Hispanic, 3=Black, 4=White, 5=Native American.}
    \item{\code{sctype}}{Type of school: 1=public, 2=catholic, 3= Private other religion, 4=Private non-r.}
    \item{\code{cstr}}{Classroom environment structure: ordinal from 1=not accurate to 5=very much accurate.}
    \item{\code{scsize}}{School size: ordinal from 1=[1,199) to 7=[1200+).}
    \item{\code{urban}}{Urbanicity: 1=Urban, 2=Suburban, 3=Rural.}
    \item{\code{region}}{Geographic region of the school: NE=1,NC=2,South=3,West=4.}
    \item{\code{schnum}}{Standardized school ID.}
  }
}
% \details{
% %%  ~~ If necessary, more details than the __description__ above ~~
% }
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
Ita G G Kreft, Jan De Leeuw 1988. Introducing Multilevel Modeling, Sage
%http://gifi.stat.ucla.edu/janspubs/1998/books/kreft_deleeuw_B_98.pdf
National Education Longitudinal Study of 1988 (NELS:88): https://nces.ed.gov/surveys/nels88/
}
% \references{
% %%  ~~ possibly secondary sources and usages ~~
% }
\examples{
data(schools)
 
# Kreft and De Leeuw, Introducing Multilevel Modeling, Sage (1988).   
# The data set is the subsample of NELS-88 data consisting of 10 handpicked schools
# from the 1003 schools in the full data set.
 
# Suppose that the effect of homeworks on math score is unconfounded conditional on X and
# unobserved school features (we assume this only for illustrative purpouse)

# Let us consider the following variables:

X<-schools$ses #X<-as.matrix(schools[,c("ses","white","public")])
Y<-schools$math
Tr<-ifelse(schools$homework>1,1,0)
Group<-schools$schid
# Note that when Group is missing, NULL or there is only one Group the function
# returns the output of the Match function with a warning.

# Let us assume that the effect of homeworks (Tr) on math score (Y)
# is unconfounded conditional on X and other unobserved schools features.
# Several strategies to handle unobserved group characteristics
# are described in Arpino & Cannas, 2016 (see References). 


# Multivariate Matching on covariates in X 
#(default parameters: one-to-one matching on X with replacement with a caliper of 0.25).

### Matching within schools
 mw<-MatchW(Y=Y, Tr=Tr, X=X, Group=Group, caliper=0.1)
 
 # compare balance before and after matching
 bmw  <- MatchBalance(Tr~X,data=schools,match.out=mw)
 
 # calculate proportion of matched observations
  (mw$orig.treated.nobs-mw$ndrops)/mw$orig.treated.nobs 
  
 # check number of drops by school
 mw$orig.ndrops.by.group 
 
 # examine output
 mw                   # complete list of results                 
 summary(mw)  # basic statistics
 
 
#### Propensity score matching

# estimate the propensity score (ps) model

mod <- glm(Tr~ses+parented+public+sex+race+urban,
family=binomial(link="logit"),data=schools)
eps <- fitted(mod)

# eg 1: within-school propensity score matching
psmw <- MatchW(Y=schools$math, Tr=Tr, X=eps, Group=schools$schid, caliper=0.1)

# We can use other strategies for controlling unobserved cluster covariates
# by using different specifications of ps (see Arpino and Mealli for details):

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
\keyword{school dataset (NELS-88)}


