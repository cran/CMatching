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
\keyword{school dataset (NELS-88)}
