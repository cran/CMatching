summary.CMatch <-
function (object, ..., full = FALSE, digits = 5) 
{

    if (!is.list(object)) {
        warning("'Match' object contains less than two valid matches. Cannot proceed.")
        return(invisible(NULL))
    }
    if (is.element("CMatch",class(object))==FALSE) {
        warning("Object not of class 'CMatch'")
        return(invisible(NULL))
    }
    else{
    	     cat("\n")
        cat("Estimate... ", format(object$est, digits = digits), 
            "\n")
        cat("SE......... ", format(object$se, digits = digits), 
            "\n")
        #cat("T-stat..... ", format(object$est/object$se, digits = digits), "\n")
        #cat("p.val...... ", format.pval((1 - pnorm(abs(object$est/object$se))) *  2, digits = digits), "\n")
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
       cat("Original number of treated obs by group...... ", "\n")
            print(object$orig.treated.nobs.by.group)
       cat("\n")
               
                   tab.drops             <- object$orig.treated.nobs.by.group
                   for (i in 1:length(tab.drops)){
   	                       tab.drops[i]  <-
   	                       object$orig.ndrops.by.group[names(object$orig.treated.nobs.by.group)[i]]
                   	}
                   tab.drops[is.na(tab.drops)] <- 0
     }
    else {
        cat("Original number of control obs............... ", 
            object$orig.nobs - object$orig.treated.nobs, "\n")
                  cat("Original number of control obs by group........ ", "\n" )
            print(object$orig.control.nobs.by.group)
             cat("\n")
                   tab.drops             <- object$orig.control.nobs.by.group
                   for (i in 1:length(tab.drops)){
   	                       tab.drops[i]  <-
   	                       object$orig.ndrops.by.group[names(object$orig.control.nobs.by.group)[i]]
                   	}
                   tab.drops[is.na(tab.drops)] <- 0
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
        cat("Number of obs dropped by 'exact' or 'caliper' by group  ", "\n") 
                  print(tab.drops)                                # table drops after within
                #print(object$orig.ndrops.by.group)  # drops after within
            } else{
            cat("Number of obs dropped by 'exact' or 'caliper by group  ", "\n")
            print(tab.drops)                                # table drops after pref within
            #print(object$orig.ndrops.by.group.after.prefwithin)  # drops after pref.within
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
            cat("Number of obs dropped by 'exact' or 'caliper' by group   ","\n")
            print(tab.drops) # tab drops after within 
            #print(object$orig.ndrops.by.group)  # drops after within
            } else{
            cat("Number of obs dropped by 'exact' or 'caliper' by group   ", "\n")
            print(tab.drops) # tab drops after pref within 
            #print(object$orig.ndrops.by.group.after.prefwithin) # drops after pref.within 
            } 

        if (object$ndrops.matches != round(object$ndrops)) 
            cat("Weighted #obs dropped by 'exact' or 'caliper' ", 
                round(object$ndrops, 3), "\n")
        cat("\n")
     }
}
    z <- list()
    class(z) <- "summary.CMatch"
    return(invisible(z))
}
