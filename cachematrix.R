## The makeCacheMatrix function will create a matrix object that can cache 
## the inverse of a matrix.  Then the cacheSolve function will compute the 
## inverse of a matrix returned by the makeCacheMatrix function.

## The makeCacheMatrix is a function to create an object (matrix class) that 
## will cache its inverse.

makeCacheMatrix <- function(X = matrix()) {
    invs <- NULL
    set <- function(y) {
      X <<- y
      invs <<- NULL
  }
    get <- function() X
    setinverse <- function(solve) invs <<- solve
    getinverse <- function() invs
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function is a function to compute the inverse of the 
## matrix returned by CacheMatrix.

cacheSolve <- function(X, ...) {
    invs <- X$getinverse()
      if(!is.null(invs)) {
      message("getting cached data")
      return(invs)
    }
    data <- X$get()
    invs <- solve(data, ...)
    X$setinverse(invs)
    invs
    ## Return a matrix (invs) that is the inverse of 'X'
}

