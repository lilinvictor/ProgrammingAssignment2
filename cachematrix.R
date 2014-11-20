## Functions to support caching of the inverse of matrix

## Make a matrix which supports caching of inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	
	# function to update stored matrix
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	
	# function to get stored matrix
	get <- function() x
	
	# function to set cached inverse
	setInverse <- function(m = matrix()) inverse <<- m
	
	# function to get cached inverse
	getInverse <- function() inverse
	
	list(set = set,
		 get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'. 
## The inverse of matrix is cached for later use.

cacheSolve <- function(x, ...) {
	# Return cached inverse if already solved
	inverse <- x$getInverse()
	if(!is.null(inverse)) {
		message("Inverse already solved, returning cached matrix")
	}
	else {
		message("Solve the inverse and save into cache")
	
		m <- x$get()
		inverse <- solve(m)
		x$setInverse(inverse)
	}
	
	return(inverse)
}
