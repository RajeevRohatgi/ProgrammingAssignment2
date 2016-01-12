## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix returns a list of four functions
##	set() sets the value of a matrix to argument x
##		The default value of this argument is a 1x1 NA matrix (see ?matrix)
##	get()	returns the value of the matrix
##	setinverse() sets the value of inverse i using the solve() function on x
##	getinverse() gets the value of the inverse i
##
##	Usage:
## Thanks to Greg Horne
## https://asitarrives.wordpress.com/2014/10/18/understanding-lexical-scoping-in-r-great-guidance-for-community-ta-in-coursera/
##	A<- [some matrix]
##	u<-makeCacheMatrix(A)
##	u$get() 			# confirm that A was set
##	u$getinverse()		# before cachesolve() => NULL, after returns cached inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL			# inverse
	set <- function (y) {
		x <<- y		# set the X to argument Y
		i <<- NULL		# clear the Inverse
	}
	get <- function() x
	setinverse <- function(solve) i <<- solve
	getinverse <- function() i
	list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
##	This function takes one argument x which should be a matrix
##	If the inverse i has already been cached in makeCacheMatrix, i is returned
##	Otherwise, the data is retrieved from the makeCacheMatrix list, and 
##		its inverse is computed using solve()
##	The inverse is cached in the makeCacheMatrix list, and also returned
##
##	Usage:
##		Note: u was created by call to makeCacheMatrix()
##	cacheSolve(u)		# first call computes inverse and caches it
##	cacheSolve(u)		# second call retrieves cached value

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'

	i <- x$getinverse();
	if(!is.null(i)) {
		message ("getting cached data")
		return (i)
	}
	data <- x$get()
	i <- solve(data)		# with no other arguments, inverse is computed
	x$setinverse(i)
	i
}
