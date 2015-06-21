## The makeCacheMatrix function returns a list of getters and setters 
## useful for caching inverse of matrix while the cacheSolve computes 
## and stores the inverse of matrix if it is not in the cache 

## The makeCacheMatrix recieves and stores a matrix together with
## its inverse for future retrieval. It returns getters and setters
## necessary to accomplish these.

makeCacheMatrix <- function(x = matrix()) {
	invs<-NULL
	set<-function(y){
		x<<-y
		invs<-NULL
		}
	get<- function() x
	setInvs<-function(inver) invs<<-inver
	getInvs<- function()invs
	list(set=set, get=get, setInvs=setInvs, getInvs=getInvs)
}


## The cacheSolve function recieves a matrix and tries to compute and return
## the inverse if it is not in the cache. it also stores the inverse 
## in the cache for future retrieval.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
	  invs <-x$getInvs()
	   if(!is.null(invs)){
		message("getting cached inverse matrix")
		return(invs)
	}
	mat <-x$get()
	invs <- solve(mat)
	x$setInvs(invs)
	invs
}
