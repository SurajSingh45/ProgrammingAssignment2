## Put comments here that give an overall description of what your
## functions do
## Create a special matrix object with caching capability
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the cache
  cache <- NULL
  
  # Function to set the matrix value and clear the cache
  set <- function(newValue) {
    x <<- newValue
    cache <<- NULL  # Clear the cache
  }
  
  # Function to retrieve the matrix value
  get <- function() {
    x
  }
  
  # Function to calculate the inverse of the matrix with caching
  cacheInverse <- function() {
    # Check if the inverse is already calculated and cached
    if (!is.null(cache)) {
      message("Fetching inverse from cache")
      return(cache)
    }
    
    # Calculate the inverse of the matrix
    inverse <- solve(x)
    
    # Cache the inverse
    cache <<- inverse
    
    message("Calculating inverse")
    return(inverse)
  }
  
  # Return a list of functions
  list(set = set, get = get, cacheInverse = cacheInverse)
}

## Function to retrieve the cached inverse of a matrix
cacheSolve <- function(x, ...) {
  # Check if the matrix has a cached inverse
  if (!is.null(x$cacheInverse())) {
    return(x$cacheInverse())
  }
  
  # If not cached, calculate the inverse
  inverse <- x$cacheInverse()
  return(inverse)
}


