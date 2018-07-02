CMatchBalance <-function (match.out, formula, data = NULL, ks = TRUE, nboots = 500, weights = NULL, digits = 5, paired = TRUE, print.level = 1) {
   UseMethod("CMatchBalance", match.out)
 }
 
CMatchBalance.CMatch<-function (match.out, formula, data = NULL, ks = TRUE, nboots = 500, weights = NULL, digits = 5, paired = TRUE, print.level = 1)
 {
 	
 class(match.out)<-"Match"	
 
 CMatchBalance.default(match.out, formula=formula, data = data, ks = ks, nboots = nboots, weights = weights, digits = digits, paired = paired, print.level = print.level)
 }
 
 # MB.Match<-function (match.out, formula, data = NULL, ks = TRUE, nboots = 500, weights = NULL, digits = 5, paired = TRUE, print.level = 1)
 # {
 	
 # MatchBalance(formula, data = data, match.out=match.out, ks = ks, nboots = nboots, weights = weights, digits = digits, paired = paired, print.level = print.level)
 # }
 
CMatchBalance.default<-function (match.out, formula, data = NULL, ks = TRUE, nboots = 500, weights = NULL, digits = 5, paired = TRUE, print.level = 1)
 {
 #warning("Matched dataset was NULL: before matching comparison only")	
 MatchBalance(formula, data = data, match.out=match.out, ks = ks, nboots = nboots, weights = weights, digits = digits, paired = paired, print.level = print.level)
 }
 
 

