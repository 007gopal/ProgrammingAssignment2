##  Functions to create object that stores a matrix and caches its inverse. 
##  Use -- Calculate inverse once and retrieve it from cache rather than
##  recomputing again


## makeCacheMatrix -- Function to create a list for setting and getting values of 
## matrix along with its inverse
makeCacheMatrix <- function(x=matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve -- Checks and gets the inverse of makeCacheMatrix if available in Cache.
## If not, it calculates the inverse and caches it for next time use.
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

