## Functions "makeCacheMatrix" and "cacheSolve" use to computing the inverse of a square matrix.
## This functions work together to create invertible matrix
## and make the inverse of the matrix available in the cache environment.
## R function "solve" is used to obtain the inverse matrix.

## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix creates and returns a list of functions
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
       x <<- y
       m <<- NULL
     }
     get <- function() x
     setinv <- function(inv) m <<- inv
     getinv <- function() m
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}

## Function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieve the inverse from the cache.
## Function returns a inverse matrix.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
