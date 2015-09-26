##this function computes the inverse of a square matrix and caches 
##also creates a special matrix that caches the inverse
makeCacheMatrix <- function(x = matrix()) {
  set <- function(y) {
    x <<- y
    i <<- NULL
    
  }
  get <- function() x 
  setinv <- function(reverse) i <<- reverse
  getinv <- function() i
  list( set = set, get= get, setinv = setinv, getinv = getinv)
}

## computes the inverse of thee special matrix returned from the makeCacheMatrix.
## if the inverse has already been calculated then cacheSolve retrieves the inverse of the cache
cacheSolve <- function(x, ...) {
  
  invFunc <- x$getinv()
  if(!is.null(invFunc)){
    message("getting cached data")
    return(invFunc)
    
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  
  x$setinv(invFunc)
  invFunc
}


