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

boolDist<-safelyDistinct(arrayDids)
colnames(boolDist)<-as.character(arrayDids)

selectedDids <- array(dim=300)
for(i in 1:300) {

	safeNumbers <- colSums(boolDist)
	
	selectedDids[i] <- names(safeNumbers[match(max(safeNumbers),safeNumbers)])
	
	arrayDids<-arrayDids[boolDist[,selectedDids[i]]]
	boolDist<-boolDist[boolDist[,selectedDids[i]],boolDist[,selectedDids[i]]]
	
}
selectedDids

"Loop:"
proc.time()-ptm