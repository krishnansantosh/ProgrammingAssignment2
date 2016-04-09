## Function makeCacheMatrix creates a special inverse matrix object that can cache its inverse. makeCacheMatrix contains 4 functions: set, get, setinverse, getinverse.
## get is a function that returns the vector x stored in the main function.
## set is a function that changes the vector stored in the main function.
## setinverse and getinverse are functions very similar to set and get.
## They don't calculate the inverse, they simply store the value of the input in a variable inv.
## into the main function makeVector (setinverse) and return it (getinverse).

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}
## This function computes the inverse of the the special "matrix" returned by makeCacheMatrix above. If the
## inverse has already been calculated (and the matrix has not changed), the cacheSolve should retrieve the inverse from
## the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}