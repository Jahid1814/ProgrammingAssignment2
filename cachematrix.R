## This is the solution to Coursera R Programming course's 2nd programming
## assignment. It caches the inverse of a matrix and returns it from cache
## when possible.

## This function enables cahceing the inverse of a matrix you create

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInverse	<- function(solve) i <<- solve
	getInverse	<- function() i
	list(set = set
		, get = get
		, setInverse = setInverse
		, getInverse = getInverse)
}


## This function calculates the inverse matrix for the matrix and caches it.
## If possible, it returns it from caches and lets you know about it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getInverse()
	if(!is.null(i)) {
		message ("getting cached data")
		return(i)
	}
	ma <- x$get()
	i <- solve(ma, ...)
	x$setInverse(i)
	i
}
