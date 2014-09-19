
makeCacheMatrix <- function(x = numeric()) {
  # holds the cached value or NULL if nothing is cached
 
  cache <- NULL
  
  setMatrix <- function(newValue) {
    x <<- newValue
    # since the matrix is assigned a new value, flush the cache
    
    cache <<- NULL
  }
  # returns the stored matrix
  
  getMatrix <- function() {
    x
  }
   
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  # get the cached value
  getInverse <- function() {
    cache
  }
  # return a list
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

# The following function calculates the inverse of a matrix created with makeCacheMatrix

cacheSolve <- function(y, ...) {
  
  # get the cached value
  
  inverse <- y$getInverse()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  #get the matrix, caclulate the inverse and store it in the cache
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
  # return the inverse
  inverse
}
