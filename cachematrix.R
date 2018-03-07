## Matrix inversion is usually a costly computation and there may be some benefit
## to chaching the inverse of a matrix. Below are two functions one which 
## will create a matrix object that can cache its inverse, and one that computes
## the inverse of the matrix returned by the first. 

## makeCacheMatirx: This function creates a special matrix that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL 
	}
	get <- function() x 
	setInverse <- function(inverse) inv <<- inverse 
	getInverse <- function() inv
	list(set = set,
	     get = get, 
	     setInverse = setInverse, 
	     getInverse - getInverse)
}


## This function computes the inverse of the special matrix created by makeCacheMatrix
## If the inverse has already been calculated, and the matrix has not changes, then it 
## should retreive the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	is(!is.null(inv)){
		message("getting cached Inverse")
		return(inv)
	}
	mat <- x$get()
	inv <= solve(mat, ...) 
	x$setInverse(inv)
	inv
}
