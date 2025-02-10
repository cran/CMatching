\name{schools}
\alias{schools}
\docType{data}
\title{
Schools data set (NELS-88)
}
\description{
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
 \details{
The data set is used in the example section to illustrate the use of functions \code{MatchW} and \code{MatchPW}. 
 }
\source{
Ita G G Kreft, Jan De Leeuw 1988. Introducing Multilevel Modeling, Sage
%http://gifi.stat.ucla.edu/janspubs/1998/books/kreft_deleeuw_B_98.pdf
National Education Longitudinal Study of 1988 (NELS:88): https://nces.ed.gov/surveys/nels88/
}
\seealso{
See also \code{\link[CMatching]{MatchW}}, \code{ \link[CMatching]{MatchPW}}
}
\examples{
data(schools)
 
# Kreft and De Leeuw, Introducing Multilevel Modeling, Sage (1988).   
# The data set is the subsample of NELS-88 data consisting of 10 handpicked schools
# from the 1003 schools in the full data set.
 
# To study the effect of the homeworks on the outcome math score, conditional on
# confounder(s) X and unobserved school features, we can define the following variables:

X<-schools$ses 
# or define a vector for more than one confounder
X<-as.matrix(schools[,c("ses","white","public")])
Y<-schools$math
Tr<-ifelse(schools$homework>1,1,0)
Group<-schools$schid

}
\keyword{school dataset (NELS-88)}


