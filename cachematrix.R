## Put comments here that give an overall description of what your
## functions do
##
## makeCacheMatrix function creates a list of functions and 
## the functions just set or get the value of a matrix
## you can put a matrix as an input and the function only stores a list of the values
##
## cacheSolve function returns the inverse of a matrix and cache the value
## you can put a list as an input(in terms of makeCacheMatrix())
## if there is a cached data, the function jsut returns it
## if there is no cached data, the function calculates the inverse of the matrix
## and caches the value

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
