summary.Match <-
function (object, ..., full = FALSE, digits = 5) 
{

    if (!is.list(object)) {
        warning("'Match' object contains less than two valid matches.  Cannot proceed.")
        return(invisible(NULL))
    }
    if (class(object) != "Match") {
        warning("Object not of class 'Match'")
        return(invisible(NULL))
    }
# object is output of Match function: same summary of summary.Match
if ( object$version!="matchwithin" & object$version!="matchprefwithin"){
    if (object$version != "fast" ) {
        cat("\n")
        cat("Estimate... ", format(object$est, digits = digits), 
            "\n")
        cat("AI SE...... ", format(object$se, digits = digits), 
            "\n")
        cat("T-stat..... ", format(object$est/object$se, digits = digits), 
            "\n")
        cat("p.val...... ", format.pval((1 - pnorm(abs(object$est/object$se))) * 
            2, digits = digits), "\n")
        cat("\n")
    }
    else {
        cat("\n")
        cat("Estimate... ", format(object$est, digits = digits), 
            "\n")
        cat("SE......... ", format(object$se.standard, digits = digits), 
            "\n")
        cat("T-stat..... ", format(object$est/object$se.standard, 
            digits = digits), "\n")
        cat("p.val...... ", format.pval((1 - pnorm(abs(object$est/object$se.standard))) * 
            2, digits = digits), "\n")
        cat("\n")
    }
    if (full) {
        cat("Est noAdj.. ", format(object$est.noadj, digits = digits), 
            "\n")
        cat("SE......... ", format(object$se.standard, digits = digits), 
            "\n")
        cat("T-stat..... ", format(object$est.noadj/object$se.standard, 
            digits = digits), "\n")
        cat("p.val...... ", format.pval((1 - pnorm(abs(object$est.noadj/object$se.standard))) * 
            2, digits = digits), "\n")
        cat("\n")
    }
    if (object$orig.wnobs != object$orig.nobs) 
        cat("Original number of observations (weighted)... ", 
            round(object$orig.wnobs, 3), "\n")
    cat("Original number of observations.............. ", object$orig.nobs, 
        "\n")
    if (object$mdata$orig.weighted.treated.nobs != object$orig.treated.nobs) 
        cat("Original number of treated obs (weighted).... ", 
            round(object$mdata$orig.weighted.treated.nobs, 3), 
            "\n")
    if (object$estimand != "ATC") {
        cat("Original number of treated obs............... ", 
            object$orig.treated.nobs, "\n")
    }
    else {
        cat("Original number of control obs............... ", 
            object$orig.nobs - object$orig.treated.nobs, "\n")
    }
    cat("Matched number of observations............... ", round(object$wnobs, 
        3), "\n")
    cat("Matched number of observations  (unweighted). ", length(object$index.treated), 
        "\n")
    cat("\n")
    if (!is.null(object$exact)) {
        cat("Number of obs dropped by 'exact' or 'caliper' ", 
            object$ndrops.matches, "\n")
        if (object$ndrops.matches != round(object$ndrops)) 
            cat("Weighted #obs dropped by 'exact' or 'caliper' ", 
                round(object$ndrops, 3), "\n")
        cat("\n")
    }
    else if (!is.null(object$caliper)) {
        cat("Caliper (SDs)........................................  ", 
            object$caliper, "\n")
        cat("Number of obs dropped by 'exact' or 'caliper' ", 
            object$ndrops.matches, "\n")
        if (object$ndrops.matches != round(object$ndrops)) 
            cat("Weighted #obs dropped by 'exact' or 'caliper' ", 
                round(object$ndrops, 3), "\n")
        cat("\n")
    }
}
    # object is output of MatchW or MatchPW: sligthly customized output
else{
    	     cat("\n")
        cat("Estimate... ", format(object$est, digits = digits), 
            "\n")
        cat("SE......... ", format(object$se, digits = digits), 
            "\n")
        cat("T-stat..... ", format(object$est/object$se, 
            digits = digits), "\n")
        cat("p.val...... ", format.pval((1 - pnorm(abs(object$est/object$se))) * 
            2, digits = digits), "\n")
        cat("\n")
        
            if (object$orig.wnobs != object$orig.nobs) 
        cat("Original number of observations (weighted)... ", 
            round(object$orig.wnobs, 3), "\n")
    cat("Original number of observations.............. ", object$orig.nobs, 
        "\n")
    if (object$mdata$orig.weighted.treated.nobs != object$orig.treated.nobs) 
        cat("Original number of treated obs (weighted).... ", 
            round(object$mdata$orig.weighted.treated.nobs, 3), 
            "\n")
    if (object$estimand != "ATC") {
        cat("Original number of treated obs............... ", 
            object$orig.treated.nobs, "\n")
       cat("Original number of treated obs by group...... ", 
            object$orig.treated.nobs.by.group, "\n")
     }
    else {
        cat("Original number of control obs............... ", 
            object$orig.nobs - object$orig.treated.nobs, "\n")
                  cat("Original number of control obs by group........ ", 
            object$orig.control.nobs.by.group, "\n")
    }
    cat("Matched number of observations............... ", round(object$wnobs, 
        3), "\n")
    cat("Matched number of observations  (unweighted). ", length(object$index.treated), 
        "\n")
    cat("\n")
    if (object$exact!=FALSE ) {
        cat("Number of obs dropped by 'exact' or 'caliper' ........  ", 
            object$ndrops.matches, "\n")
            if(object$version=="matchwithin"){
        cat("Number of obs dropped by 'exact' or 'caliper' by group  ", 
            object$orig.dropped.nobs.by.group, "\n")  
            } else{
            #cat("Number of obs dropped by 'exact' or 'caliper'\n","by group after within ...................... ", 
            #object$orig.dropped.nobs.by.group.after.within, "\n")  
            cat("Number of obs dropped by 'exact' or 'caliper by group  ", 
            object$orig.dropped.nobs.by.group.after.prefwithin, "\n")  
           } 
        if (object$ndrops.matches != round(object$ndrops)) 
            cat("Weighted #obs dropped by 'exact' or 'caliper' ", 
                round(object$ndrops, 3), "\n")
        cat("\n")
    }
    else if (!is.null(object$caliper)) {
        cat("Caliper (SDs)..........................................  ", 
            object$caliper, "\n")
        cat("Number of obs dropped by 'exact' or 'caliper' .........  ", 
            object$ndrops.matches, "\n")
            if(object$version=="matchwithin"){
            cat("Number of obs dropped by 'exact' or 'caliper' by group   ", 
            object$orig.dropped.nobs.by.group, "\n")  
            } else{
            #cat("Number of obs dropped by 'exact' or 'caliper'\n","by group after within ...................... ", 
            #object$orig.dropped.nobs.by.group.after.within, "\n")  
            cat("Number of obs dropped by 'exact' or 'caliper' by group   ", 
            object$orig.dropped.nobs.by.group.after.prefwithin, "\n")  
            } 

        if (object$ndrops.matches != round(object$ndrops)) 
            cat("Weighted #obs dropped by 'exact' or 'caliper' ", 
                round(object$ndrops, 3), "\n")
        cat("\n")
     }

    	
}
    
    z <- list()
    class(z) <- "summary.Match"
    return(invisible(z))
}

print.summary.Match <- function(x, ...)
  {
    invisible(NULL)
  }
