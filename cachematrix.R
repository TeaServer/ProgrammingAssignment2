## The functions below are built using the example provided for mean of a vector
## The mean function was changed to be the 'solve' function to get the inverse of
## the initial matrix

## This function creates the initial cache of matrix x
## 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}

## This function computes the inverse of the matrix and stores the values for later retrieval
## If the inverse has already been computed, the result is returned (without re-computing)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
