## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## From assignment description:
## The following function creates a special "matrix" object that can 
## cache its inverse. This is mostly derived from the provided makeVector
## function.

makeCacheMatrix <- function(x = matrix()) {
	# Initializing holder for storing resulting inverse matrix.
	holder <- NULL

	# Sets matrix.
	setMatrix <- function(y) {
		x <<- y
		holder <<- NULL
	}

	# Gets matrix.
	getMatrix <- function() x

	# Sets inverse matrix.
	setInverseMat <- function(inverse) holder <<- inverse

	# Gets inverse matrix.
	getInverseMat <- function() holder

	# Returns list.
	list(setMatrix = setMatrix, getMatrix = getMatrix,
		setInverseMat = setInverseMat, getInverseMat = getInverseMat)
}


## Write a short comment describing this function
## From assignment description:
## Function calculates the mean of the special "vector" created with the 
## above function. However, it first checks to see if the mean has already 
## been calculated. If so, it gets the mean from the cache and skips the 
## computation. Otherwise, it calculates the mean of the data and sets the 
## value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	# Gets inverse matrix.
	cache <- NULL
	mat <- x$getInverseMat()

	# Checks if mean calculation exists in cache.
	if (is.null(mat)) {

	#### For testing purposes only please ignore the following lines.
	#### with "####"
	#### if (is.null(mat) || !missing(...)) {
	####	message("writing to cache")	
		
		# Calculates mean and cache result if it doesn't already exist.
		cache <- x$setInverseMat(solve(x$getMatrix(), ...))

		# Returns result.	
		return(cache)
	}

	# Runs the following when cache result is available.
	message("getting cached data")

	# Returns result from cache if it exists.
	return(mat)
}