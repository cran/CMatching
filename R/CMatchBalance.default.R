CMatchBalance.default <-
function (match.out, formula, data = NULL, ks = TRUE, nboots = 500, weights = NULL, digits = 5, paired = TRUE, print.level = 1)
 {
 #warning("Matched dataset was NULL: before matching comparison only")	
 MatchBalance(formula, data = data, match.out=match.out, ks = ks, nboots = nboots, weights = weights, digits = digits, paired = paired, print.level = print.level)
 }
