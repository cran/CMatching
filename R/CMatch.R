CMatch <-
function (type,Y=NULL, Tr, X,
Group=NULL,estimand="ATT",M=1,exact=NULL,caliper=0.25,
weights=NULL,replace=TRUE,ties=TRUE,...) {
   if (type!="within" & type!="pwithin"){stop("type must be one of < within > or < pwithin > ")}  
         if (any(Group%%1==0)==FALSE & is.null(Group)==FALSE){
       	     stop("The variable Group should be integer valued")
       	}
         if (is.unsorted(Group)==TRUE) { 
         	stop("The variable Group is not in ascending order. Please reorder the input (X,Tr,Y, Group) in ascending order of Groupp.")
         	}
   else{
   	if (type=="within"){  		
   	return(MatchW(Y, Tr, X, Group,estimand,M,exact,caliper,weights,replace,ties,...))	
   	}
   	if (type=="pwithin"){ 		
   	return(MatchPW(Y=Y, Tr=Tr, X=X, Group=Group,estimand=estimand,M=M,exact=exact,caliper=caliper,weights=weights,replace=replace,ties=ties,...))	
   	}
   	}}
