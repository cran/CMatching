MatchPW <-
function(Y=NULL, Tr, X, Group=NULL,estimand="ATT",M=1, exact=NULL,caliper=0.25,replace=TRUE,ties=TRUE,weights=NULL,...) 
{
# Match units first within cluster (defined by the variable Group) and then match the unmatched between clusters.

# check missing arguments	
  if (replace==FALSE)
  {warning("Match preferential within not possible with replace==FALSE. Resetting to the default, which is TRUE")}
    
     if (is.null(weights)) {
     	weights <- rep(1, length(Tr))} else {
        weights <- as.double(weights)
        } 
     if(is.null(exact)) {exact<-FALSE}
            if (any(Group%%1==0)==FALSE & is.null(Group)==FALSE){
       	print("The variable Group should be integer valued")
       	stop
       	}
       	  if (is.unsorted(Group)==TRUE) { print("The variable Group is not in ascending order. Please reorder the input (X,Tr,Y, Group) in ascending order of Group."); stop
       	  	  # ordg<-order(Group)
       	      # Group<-Group[ordg] ; Y<-Y[ordg]; Tr<-Tr[ordg] ; X<-as.matrix(as.matrix(X)[ordg,])
       	       }
       	  if (is.null(Group)){
        	Group<-rep(1, length(Tr))
        	}
        	Gmax <- length(unique(Group))#dim(table((Group)));
          if (Gmax==1){
        	warning("There is only one group: same output of Match")
        	B<-Match(Y,Tr,X,estimand=estimand,M=1,exact=exact,caliper=caliper,
weights=weights,replace=replace,ties=ties)
        	return(B)
        	stop
        	}
# discriminate data with and without Y
 Y.orig  <- Y
 Y        <- if(is.null(Y.orig)){Y<-rep(0,length(Tr))}else{Y<-Y.orig}
 
 # first Match within
 MW<- MatchW(Y, Tr, X, Group,
 estimand=estimand,M=M,exact=exact,caliper=caliper,
 replace=TRUE,ties=ties)
     
     if (length(MW$index.dropped)==0){
     	warning("all treated units matched within group")
     	return(MW)
     	stop
     	}
                   
# then create dataset with unmatched within and all treated/controls
if (estimand=="ATT"){
        X  <-  as.matrix(X)        
       Yu  <-  c( Y[MW$index.dropped], Y[Tr==0])
      Tru  <-  c(Tr[MW$index.dropped], Tr[Tr==0])
       Xu  <-  rbind(as.matrix(X[c(MW$index.dropped),  ]),
       as.matrix(X[Tr==0, ]))
       Wu  <-  c(weights[MW$index.dropped], weights[Tr==0])
       }

if (estimand=="ATC"){
        X  <-  as.matrix(X)        
       Yu  <-  c( Y[MW$index.dropped], Y[Tr==1])
      Tru  <-  c(Tr[MW$index.dropped], Tr[Tr==1])
       Xu  <-  rbind(as.matrix(X[c(MW$index.dropped), ]),
       as.matrix(X[Tr==1, ]))
       Wu  <-  c(weights[MW$index.dropped], weights[Tr==1])
       } 
                   
# calculate caliper
         ucaliper<-MW$intcaliper


          # Match between
          btw <- Match(Y = Yu, Tr = Tru, X = Xu, weights=Wu,
          estimand=estimand,M=M,exact=exact,caliper= ucaliper,replace=TRUE,ties=ties)
                                               
           if (estimand=="ATE"){
   ind.unmatched.treated <-MW$index.dropped[Tr[MW$index.dropped]==1]
   ind.unmatched.controls<-MW$index.dropped[Tr[MW$index.dropped]==0]
        X  <-  as.matrix(X)        
       Yut  <-  c( Y[ind.unmatched.treated], Y[Tr==0])
      Trut  <-  c(Tr[ind.unmatched.treated], Tr[Tr==0])
       Xut  <-  rbind(as.matrix(X[c(ind.unmatched.treated), ]),
       as.matrix(X[Tr==0,]))
       Wut  <-  c(weights[ind.unmatched.treated], weights[Tr==0])       
       
              Yuc  <-  c( Y[ind.unmatched.controls], Y[Tr==1])
      Truc  <-  c(Tr[ind.unmatched.controls], Tr[Tr==1])
       Xuc  <-  rbind(as.matrix(X[c(ind.unmatched.controls), ]),
       as.matrix(X[Tr==1,]))
       Wuc  <-  c(weights[ind.unmatched.controls], weights[Tr==1])       
       
       btwt <- Match(Y = Yut, Tr = Trut, X = Xut,weights=Wut, 
          estimand="ATT",exact=exact,caliper= ucaliper,replace=TRUE,ties=ties)
       btwc <- Match(Y = Yuc, Tr = Truc, X = Xuc, weights=Wuc,
          estimand="ATC",exact=exact,caliper= ucaliper,replace=TRUE,ties=ties)
       } 

# find index of matched between in case of no match

          if (is.na(btw[1])) {
          	  btw<-list()
              btw$index.dropped <- (1:length(MW$index.dropped))
              btw$index.control <- NULL
              btw$index.treated <- NULL
              btw$weights       <- NULL
              }

# initialize ouput
        BB <- NULL 
                    
# add indexes of matches between (from MW) to indexes of matches between
# aggiungo ad indici dei match within gli indici dei match between
 if (estimand=="ATT"){
          BB$index.control <- 
          c(MW$index.control, which(Tr==0)[btw$index.control-length(MW$index.dropped)])
          BB$index.treated <- 
          c(MW$index.treated, MW$index.dropped[btw$index.treated])                   
          BB$index.dropped <- MW$index.dropped[btw$index.dropped]
          BB$weights <- c(MW$weights, btw$weights) 
          }
 if (estimand=="ATC"){
          BB$index.control <- 
          c(MW$index.control, which(Tr==0)[btw$index.control-length(MW$index.dropped)])
          BB$index.treated <- 
          c(MW$index.treated, MW$index.dropped[btw$index.control])                    
          BB$index.dropped <- MW$index.dropped[btw$index.dropped]
          BB$weights <- c(MW$weights, btw$weights) 
          }
 if (estimand=="ATE"){
          BB$index.control <- c(MW$index.control, ind.unmatched.controls[btwc$index.control] )
          BB$index.treated <- c(MW$index.treated, ind.unmatched.treated[btwt$index.treated] )
          BB$index.dropped <- MW$index.dropped[c(btwt$index.dropped, btwc$index.dropped)]
          BB$weights       <- c(MW$weights[MW$index.treat], btwt$weights, MW$weights[MW$index.contr], btwc$weights)
                      }
          

# VALUE (output statistics)
        # dataset matchati
              mdata <- list()
              mdata$Y  <- c(Y[BB$index.treated], Y[BB$index.control])
              mdata$Tr <- c(Tr[BB$index.treated], Tr[BB$index.control])
              mdata$X  <- rbind(X[BB$index.treated, ], X[BB$index.control, ])
              BB$mdata<-mdata
        # the causal estimand:
        BB$est <-
        sum((Y[BB$index.treated] - Y[BB$index.control])*BB$weights)/sum(BB$weights)
         if (estimand=="ATE"){
         BB$est <-
        sum((Y[BB$index.treated]*(c(MW$weights, btwt$weights))-Y[BB$index.control]*(c(MW$weights, btwc$weights))))/sum(c(MW$weights,sum(btwc$weights,btwt$weights)/2))        	
         }
         if (estimand!="ATE"){
        # Define the covariance and estimate the ATT and its (model) SE
        m0 <- 
        lm(c(Y[BB$index.control], Y[BB$index.treat]) ~ c(Tr[BB$index.control], Tr[BB$index.treat]), weights=c(BB$weights, BB$weights))}
        if (estimand=="ATE"){
        # Define the covariance and estimate the ATT and its (model) SE
        m0 <- 
        lm(c(Y[BB$index.control], Y[BB$index.treat]) ~ c(Tr[BB$index.control], Tr[BB$index.treat]), weights=BB$weights)}
        
        m0.vcovCL <- 
        cluster.vcov(m0, c(Group[BB$index.control], Group[BB$index.treat]))
        #the model estimated standard error of the causal estimand:
        BB$se <- if(is.null(Y.orig)){BB$se<-"NULL"} else {BB$se<-coeftest(m0, m0.vcovCL)[4]};
         
# descriptive stats general

# the original number of observations in the dataset:
        BB$orig.nobs <- length(Tr)
# the original number of weighted observations in the dataset:
        BB$orig.wnobs <- sum(weights)
        BB$orig.weighted.treated.nobs<-BB$mdata$orig.weighted.treated.nobs<-sum(weights[Tr==1])
        BB$orig.weighted.control.nobs<-BB$mdata$orig.weighted.control.nobs<-sum(weights[Tr==0])
# the original number of treated observations in the dataset:
        BB$orig.treated.nobs <- sum(Tr==1)
# the original number of control observations in the dataset:
        BB$orig.control.nobs <- sum(Tr==0)
# the (weighted) number of observations in the matched dataset:
        BB$wnobs = sum(BB$weights) 
        # the caliper used (in global standard deviation units for each variable) 
        if (is.null(caliper))  BB$caliper <- NULL;
        if (!is.null(caliper)) BB$caliper <- caliper;
        BB$exact = MW$exact
# the number of matches which were dropped because of the exact or caliper options
     if (estimand == "ATT") {
        actual.drops <- BB$orig.weighted.treated.nobs - BB$wnobs }
    else if (estimand == "ATC") {
        actual.drops <- BB$orig.wnobs - BB$wnobs}
    else {
        actual.drops <- (BB$orig.wnobs - BB$orig.weighted.treated.nobs) - 
            BB$wnobs }
        BB$ndrops <- actual.drops 
        BB$ndrops.matches = length(BB$index.dropped)
# the estimated parameter
        BB$estimand = MW$estimand
             
# descriptive stats by group 
 
# the original number of treated observations by group in the dataset:  
         BB$orig.treated.nobs.by.group<- MW$orig.treated.nobs.by.group
# the original number of treated observations by group in the dataset:
        #BB$orig.control.nobs.by.group<- MW$orig.control.nobs.by.group
        
# the number of dropped observations by group after within group matching
         BB$orig.ndrops.by.group.after.within<-MW$orig.ndrops.by.group
        #<-table(Group[MW$index.dropped])
         
# the numb. of dropped observations by group after preferential within group matching
         #BB$orig.ndrops.by.group.after.prefwithin<-
         BB$orig.ndrops.by.group<-table(Group[BB$index.dropped])
         
        # Some extra info
        #BB$MatchLoopC = MW$MatchLoopC
        BB$version <- "matchprefwithin" 
       
        #return output
        class(BB)    <- "Match"
        return(BB) 
 }
