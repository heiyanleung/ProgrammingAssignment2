## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
## In this assignment, I write a pair of functions that cache the inverse of a matrix.

## The first function creates a special "matrix" which really a list containing a function that can cache its inverse.
## x is a square invertible matrix, function returns a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
## this list is then used as the input to cacheSolve(), the second function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by our first fucntion, makeCacheMatrix.
## It first checks to see if the inverse has already been calculated. If so, then function cacheSolve will retrieve the inverse 
## from the cache. Otherwise, it calculates the inverse and sets the value of the inverse in the cache via the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache 
    message("getting cached data")
    return(inv)
  }
  
  # if inverse was not calculated 
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
  
  # sets the value of the inverse
  x$setinv(inv)
  
  return(inv)
}
