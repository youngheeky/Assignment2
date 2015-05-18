## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL 
	set <- function(y) {
		x <<- y
		m <<- NULL
	} ## set the value of the matrix
	get <- function() x ##get the value of the matrix
	setInverse <- function(inverse) m <<- inverse ##set the value of the inverse
	getInverse <- function() m ##get the valute of the inverse
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ##make a list of 4 functions(set, get, setInverse, getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse() ##x is a list containing 4 functions and give m the value of getInverse in the list x
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	} ##if m is not NULL, print "getting cached data" and the cached value of m
	##if m is NULL, calculate the inverse of the matrix
	data <- x$get() ##set data to the value of the matrix
	m <- solve(data,...) ##calculate the inverse of the matrix and set m to it
	x$setInverse(m) ##cache the value of the inverse
	m ##return the inverse of the matrix
}
