parseAddress <- function(x){
	if(!is.character(x)) x <- as.character(x)
	Number <- character(n <- length(x))
	FlatNumber <- character(n)
	houseName <- character(n)
	street <- character(n)
	isAFlat <- grep("([Ff][Ll][Aa][Tt]|[Rr][Oo][Oo][Mm]|[Aa][Pp][Aa][Rr][Tt][Mm][Ee][Nn][Tt]|[Aa][Pp][Tt]\\.?) ?[0-9]+[0-9a-zA-Z]?",x)
	if(!identical(isAFlat,integer(0))){
		FlatNumber[isAFlat] <- gsub("([Ff][Ll][Aa][Tt]|[Rr][Oo][Oo][Mm]|[Aa][Pp][Aa][Rr][Tt][Mm][Ee][Nn][Tt]|[Aa][Pp][Tt]\\.?) *([0-9]+[0-9a-zA-Z]?).*","\\2",x[isAFlat])
		x[isAFlat] <- gsub("([Ff][Ll][Aa][Tt]|[Rr][Oo][Oo][Mm]|[Aa][Pp][Aa][Rr][Tt][Mm][Ee][Nn][Tt]|[Aa][Pp][Tt]\\.?) *[0-9]+[0-9a-zA-Z]?[, ](.*)","\\2",x[isAFlat])
	}
	hasNumber <- grep("[0-9]",x)
	Number[hasNumber] <- gsub(".*?([0-9\\/-]+[a-zA-Z]?).*","\\1",x[hasNumber])
	x[hasNumber] <- gsub("[0-9\\/-]+[a-zA-Z]?","#NUMBER#",x[hasNumber])
	street[hasNumber] <- gsub(".*#NUMBER#[ ,]*(.*)","\\1",x[hasNumber])
	houseName[hasNumber] <- gsub("(.*?)[ ,]*#NUMBER#.*","\\1",x[hasNumber])
	if(identical(hasNumber,integer(0)))ind2 <- seq_along(x) else ind2 <- -hasNumber
	Number[ind2] <- NA
	hasCommaNotNumber <- grep("[Hh][Oo][Uu][Ss][Ee]|[Cc][Oo][Tt][Tt][Aa][Gg][eE]|,",x[ind2])
	street[ind2][hasCommaNotNumber] <- gsub("(.*?)([Hh][Oo][Uu][Ss][Ee]|[Cc][Oo][Tt][Tt][Aa][Gg][eE]|,).*?([^, ][^,]*)","\\3",x[ind2][hasCommaNotNumber])
	houseName[ind2][hasCommaNotNumber] <- gsub("(.*?)([Hh][Oo][Uu][Ss][Ee]|[Cc][Oo][Tt][Tt][Aa][Gg][eE]|,).*[^, ][^,]*","\\1\\2",x[ind2][hasCommaNotNumber])
	if(identical(hasCommaNotNumber,integer(0))) ind3 <- ind2 else ind3 <- seq_along(x)[ind2][-hasCommaNotNumber]
	street[ind3] <- gsub(".* ([A-Za-z']* [A-Za-z']*)","\\1",x[ind3])
	houseName[ind3] <- gsub("(.*) [A-Za-z']* [A-Za-z']*","\\1",x[ind3])
	return(list(FlatNumber=FlatNumber,Number=Number,houseName=houseName,street=street))
}