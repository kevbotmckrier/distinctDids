arrayDids<-read.csv("dids.csv")[[1]]

myDist<-function(a,b){
	return(sum(abs(a-b)>1)>1)
}
checkList<-function(number,numbers){
	return(sapply(numbers,myDist,number))
}
safelyDistinct<-function(arrayDids){
	
	splitList<-lapply(strsplit(as.character(arrayDids),""),as.numeric)
	
	return(sapply(splitList,checkList,splitList))
	
}

ptm <- proc.time()

findDistinctDids<-function(arrayDids,numToFind){
	
	boolDist<-safelyDistinct(arrayDids)
	colnames(boolDist)<-as.character(arrayDids)
	
	safeNumbers <- colSums(boolDist)
	selectedDid<-names(safeNumbers[match(max(safeNumbers),safeNumbers)])
	if(numToFind>1) {
		return(c(selectedDid,findDistinctDids(arrayDids[boolDist[,selectedDid]],numToFind-1)))
		
	} else {
		return(names(safeNumbers[match(max(safeNumbers),safeNumbers)]))
	}		
	
}

findDistinctDids(arrayDids,100) 
"Recursive:"
proc.time()-ptm

ptm <- proc.time()

selectedDids <- array(dim=100)
for(i in 1:100) {
	
	boolDist<-safelyDistinct(arrayDids)
	colnames(boolDist)<-as.character(arrayDids)
	
	safeNumbers <- colSums(boolDist)
	
	selectedDids[i] <- names(safeNumbers[match(max(safeNumbers),safeNumbers)])
	
	arrayDids<-arrayDids[boolDist[,selectedDids[i]]]
	
}
selectedDids

"Loop:"
proc.time()-ptm