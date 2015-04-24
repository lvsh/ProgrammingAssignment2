## The functions below are used to create a special object (a matrix)
## and to cache its inverse, based on the mean vector example given

## This function creates a special "matrix", i.e., a list of 
## functions to set value of matrix, get value of matrix,
## set value of inverse, and get value of inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function calculates the inverse of the special
## "matrix" created with the above function if the inverse
## hasn't already been computed. If the inverse has been 
## computed, it returns the cached value.
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
