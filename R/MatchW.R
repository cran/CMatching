MatchW <-
function(Y=NULL, Tr, X,
Group=NULL,estimand="ATT",M=1,exact=NULL,caliper=0.25,
weights=NULL,replace=TRUE,ties=TRUE,...) {
# match units within clusters defined by the variable Group. 
	  	   # check missing input variables

       if (any(Group%%1==0)==FALSE & is.null(Group)==FALSE){
       	print("The variable Group should be integer valued")
       	stop
       	}
         if (is.unsorted(Group)==TRUE) { print("The variable Group is not in ascending order. Please reorder the input (X,Tr,Y, Group) in ascending order of Group."); stop
         	 # ordg<-order(Group)
       	     # Group<-Group[ordg] ; Y<-Y[ordg]; Tr<-Tr[ordg] ; X<-as.matrix(as.matrix(X)[ordg,])
           	  }
        if (is.null(weights)) {
        	weights <- rep(1, length(Tr))} else {weights <- as.double(weights)
        	}
        if (is.null(exact)) exact<-FALSE 
        
        if (is.null(Group)){
        	Group<-rep(1, length(Tr))
        	}
        Gmax <- length(unique(Group))#dim(table((Group)));
        if (Gmax==1){
        	warning("There is only one group: same output of Match")
        	B<-Match(Y,Tr,X)
        	return(B)
        	stop
        	}
        

        X<-as.matrix(X)               
        # Split the data set according to the Group variable
        Y2   <- split(Y, Group)
        Tr2  <- split(Tr, Group)
        X2   <- split(as.data.frame(X), Group);X2<-sapply(X2,as.matrix)
        W2  <- split(weights, Group)
        
        # Initialize results
        B <- NULL
        #intcaliper<-vector()
        
        # define index of group cumulated frequencies       
        Index <- as.numeric(c(0, cumsum(table(Group))))
    
        for (i in 1:Gmax){
        # calculate Caliper of the Match function: 
        # defined so that it is a fixed proportion of the OVERALL sd of each variable
               if (is.null(caliper)) intcaliper[i]  <- NULL
                          
               if (!is.null(caliper)) {
             intcaliper <- vector(mode = "numeric", length = ncol(X))
               for (j in 1:ncol(X)) {
        	    mnX    <- sum(X[, j] * weights)/sum(weights)
                stdX   <- sqrt(sum((X[, j] - mnX)^2)/sum(weights))
                meanX2 <- sum(X2[[i]][, j] * W2[[i]])/sum(W2[[i]])
                sdX2   <- sqrt(sum((X2[[i]][, j] - meanX2)^2)/sum(W2[[i]]))
                intcaliper[j] <- caliper * stdX/sdX2
                               }      
                                 }
 
          # Match 
          b <- Match(Y = Y2[[i]], Tr = Tr2[[i]], X = X2[[i]], 
          estimand=estimand,M=M,exact=exact,caliper= intcaliper,
          replace=replace,ties=ties)
          
          # Define the indexes in case of zero matches 
          
          if (is.na(b[1])) {
          	    b<-list()
          	if (estimand=="ATT"){
          		b$index.dropped <- (1:length(Tr2[[i]]))[Tr2[[i]]==1]
          		}
            if (estimand=="ATC"){
            	b$index.dropped <- (1:length(Tr2[[i]]))[Tr2[[i]]==0]
            	}
            if (estimand=="ATE"){
                b$index.dropped <- (1:length(Tr2[[i]]))}
                b$index.control <- NULL
                b$index.treated <- NULL
                b$weights       <- NULL
              }
  #        if (!is.null(names(b))) {
  #            b1.dropped <- b$index.dropped# this is NULL if no drops
  #        }
                   
          # Update indexes of matches
          B$index.control <- 
          c(B$index.control, b$index.control + Index[i])
          B$index.treated <- 
          c(B$index.treated, b$index.treated + Index[i])
          B$index.dropped <- 
          c(B$index.dropped, b$index.dropped + Index[i])
          B$weights <- c(B$weights, b$weights)  
         }

        # Estimate the ATT
        B$est <-
        sum((Y[B$index.treated] - Y[B$index.control])*B$weights)/sum(B$weights)
        # Define the covariance and estimate the ATT (model) SE
        m0 <- 
        lm(c(Y[B$index.control], Y[B$index.treat]) ~ c(Tr[B$index.control], Tr[B$index.treat]), weights = c(B$weights, B$weights))
        m0.vcovCL <- 
        cluster.vcov(m0, c(Group[B$index.control], Group[B$index.treat]))
        B$se <- coeftest(m0, m0.vcovCL)[4]
        
        # the matched datasets
         mdata <- list()
              mdata$Y  <- c(Y[B$index.treated], Y[B$index.control])
              mdata$Tr <- c(Tr[B$index.treated], Tr[B$index.control])
              mdata$X  <- rbind(X[B$index.treated, ], X[B$index.control, ])
         B$mdata<-mdata
        
                #descriptive stats general
        # the original number of observations in the dataset:
        B$orig.nobs   <- length(Tr)
        # the original number of weighted observations in the dataset:
        B$orig.wnobs <- sum(weights)
        B$orig.weighted.treated.nobs<-B$mdata$orig.weighted.treated.nobs<-sum(weights[Tr==1])
        B$orig.weighted.control.nobs<-B$mdata$orig.weighted.control.nobs<-sum(weights[Tr==0])
        # the original number of treated observations in the dataset:
        B$orig.treated.nobs <- sum(Tr==1)
        B$orig.control.nobs <- sum(Tr==0)
        # the number of weighted observations in the dataset:
        B$wnobs <- sum(B$weights) 
        #the caliper used. This caliper is interpreted standard deviation units of the pooled dataset
        B$caliper<-ifelse(is.null(caliper),NULL,caliper)
        # the internal caliper used
        B$intcaliper<-intcaliper
        # the value of the exact argument
        B$exact <- b$exact
        # the number of matches dropped either because of the caliper or exact option
        if (estimand == "ATT") {
        actual.drops <- B$orig.weighted.treated.nobs - B$wnobs }
    else if (estimand == "ATC") {
        actual.drops <- B$orig.wnobs - B$wnobs}
    else {
        actual.drops <- (B$orig.wnobs - B$orig.weighted.treated.nobs) - 
            B$wnobs }
        B$ndrops <- actual.drops #sum(B$weights[B$index.dropped])??
        B$ndrops.matches <- length(B$index.dropped)
        # the estimated parameter
        B$estimand <- b$estimand
        
        #descriptive stats by group
        
        # the original number of treated observations by group in the dataset:
        B$orig.treated.nobs.by.group<-table(Group[Tr==1])
        # the original number of control observations by group in the dataset:
        #B$orig.control.nobs.by.group<-table(Group[Tr==0])
        # the number of dropped observations by group in the dataset:
        B$orig.dropped.nobs.by.group<-table(Group[B$index.dropped])
        

          
        # Some extra info
        #B$MatchLoopC = b$MatchLoopC
        B$version = "matchwithin"
        #return output
        class(B) <- "Match"
        return(B) 
      }
