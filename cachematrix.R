##  The two functions below can be used to cache the inverse of a matrix to 
##  avoid having to recalculate the inverse thus saving processing time.


## This function creates a special matrix object that can cache its inverse.  
## It returns a list containing funtions to 
## a. set the value of the matrix
## b. get the value of the matrix
## c. set the inverse of the matrix
## d. get the inverse of the matrix

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

## This function returns the inverse of a special matrix created in the above function.
## For example:   y<-makeCacheMatrix(x)
##                cacheSolve(y) returns the inverse of matrix x 
## However, to save on processing time it first checks if the inverse of the matrix has 
## been calculated. If it is, it returns the saved value from the cache. Otherwise
## the inverse of the matrix is calculated, the value saved in cache and then returned.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}