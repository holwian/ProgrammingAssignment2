## Functions supporting maintenance of a cached variable 
## to store the inverse value of a matrix.

## makeCacheMatrix is a function returning a list of functions to allow
## maintenance of a matrix variable holding the inverse of 
## a matrix.  
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


## cacheSolve returns the inverse of the matrix which is passed in 
## as an object defined by the makeCacheMatrix function.  The cached 
## result is returned if it exists.  It is assumed that an inverse to
## the passed matrix exists.

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
