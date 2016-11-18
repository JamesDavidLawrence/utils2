symmetricOuter <- function(V,FUN,...){
	FUN <- match.fun(FUN)
	if(length(dim(V)) > 1){
		warning("first argument to symmetricOuter should be a 1-D vector, coercing to same")
		V <- as.vector(V)
	}
	if(identical(FUN,`+`) || identical(FUN,`*`)){
		warning("symmetricOuter is slower than outer with FUN=\"+\" or \"*\". Using outer() instead")
		return(outer(V,V,FUN))
	}
	i <- rep.int(seq_along(V),seq_along(V))
	j <- sequence(seq_along(V))
	M <- matrix(NA,length(V),length(V))
	M[cbind(i,j)] <- M[cbind(j,i)] <- FUN(i,j,...)
	if(!is.null(names(V))) rownames(M) <- colnames(M) <- names(V)
	return(M)
}
