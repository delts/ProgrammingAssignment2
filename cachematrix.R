## The following pair of functions ('makeCacheMatrix' and 'cacheSolve')
## will cache the inverse of a matrix.  It benefits us to cache the
## the inverse of a matrix rather than rather than computing it
## repeatedly.  Through manipulation of scoping rules, we can create
## functions to save time while doing this matrix work.

## The first function is 'makeCacheMatrix'. This function creates a 
## special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matinv <<- inverse
  getinverse <- function() matinv
  list(set=set,
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}

## The second function is 'cacheSolve'.  This function computes the
## inverse of the special "matrix" returned by `makeCacheMatrix` above.
## If the inverse has already been calculated (and the matrix has not 
## changed), then this should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  matinv <- x$getinverse()
  if(!is.null(matinv)) {
    message("Retrieving Cached Data.")
    return(matinv)
  }
  data <- x$get()
  matinv <- solve(data)
  x$setinverse(matinv)
  matinv
## This should return a matrix that is the inverse of 'x' above.
}
