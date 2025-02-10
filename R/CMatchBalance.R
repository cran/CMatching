CMatchBalance <-
function (match.out, formula, data = NULL, ks = TRUE, nboots = 500, weights = NULL, digits = 5, paired = TRUE, print.level = 1) {
   UseMethod("CMatchBalance", match.out)
 }
