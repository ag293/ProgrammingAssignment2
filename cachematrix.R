## Functions for creating and computing the matrix inverse through caching
## If the cached inverse exists then matrix inverse is skipped and the cached version is returned
## functions do

## Making a special "matrix" that returns a list of functions for 
## getting, setting, getting inverse and setting inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL
	}
	print(x)
	get <- function()x
	setInv <- function(matInv) inv <<-matInv
	getInv <-function() inv
	list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## Cached matrix inverse computation
## If inverse exists in the cache, then the inverse is returned
## else the inverse is computed, set, and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          cacheSolve <- function(x, ...) {
          inv<-x$getInv()
	  if (!is.null(inv)) {
			message("getting cached data")
			return(inv)
	   }
	   data <-x$get()
	   inv <-solve(data,...)
	   x$setInv(inv)
	   inv
}
