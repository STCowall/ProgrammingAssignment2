## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## The function creates a special "matrix" object that can cache
## its inverse.

	inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL
	}
	get<-function()x
	setInverse<-function(inverse)inv<<-inverse
	getInverse<-function()inv
	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## This function computes the inverse of the special "matrix" returned
        ## by the makeCacheMatrix function. If the inverse has already been 
        ## calculated (and the matrix has not changed), then the cachesolve
        ## retrieves the inverse from the cache.

	inv<-x$getInverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data<-x$get()
	inv<-solve(data,...)
	x$setInverse(inv)
	inv
}
