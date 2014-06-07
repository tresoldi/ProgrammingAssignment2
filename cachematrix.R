## R Programming - Assignment 2

## This module implements two functions that allow to cache a computationally
## intensive operation, i.e., matrix inversion.
##
## Example:
## > source("cachematrix.R") # loads the module
## > c=rbind(c(1,-1/4),c(-1/4,1)) # sample matrix
## >
## > cacheSolve(m) # first run
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## >
## > cacheSolve(m) # second run
## getting cached matrix inversion
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## This function creates a special "matrix" object (actually, a list) that
## will cache its inverse when the complementary function cacheSolve (below)
## is used. Caching makes sense as it usually is a costly computation.
##
## The only argument, 'x', is expected to be a matrix, defaulting to an
## empty matrix if no argument is provided.
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
} 

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above; if the inverse has already been calculated (and
## the matrix has not changed) the cached inversion is returned.
##
## The only argument is 'x', the special matrix returned by
## makeCacheMatrix. The ... is kept as per rprog-004 instructions.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached matrix inversion")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}
