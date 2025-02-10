CMatchBalance.CMatch <-
function (match.out, formula, data = NULL, ks = TRUE, nboots = 500, weights = NULL, digits = 5, paired = TRUE, print.level = 1)
 {
 	
 class(match.out)<-"Match"	
 
 CMatchBalance.default(match.out, formula=formula, data = data, ks = ks, nboots = nboots, weights = weights, digits = digits, paired = paired, print.level = print.level)
 }
