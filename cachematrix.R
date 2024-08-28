
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL         # Initialize the inverse as NULL
  
  set <- function(y) {
    x <<- y           # Set the matrix
    inv <<- NULL      # Reset the inverse cache
  }
  
  get <- function() x              # Return the matrix
  setinverse <- function(inverse) inv <<- inverse  # Set the inverse
  getinverse <- function() inv     # Return the cached inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()   # Get the cached inverse
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)           # Return the cached inverse if it exists
  }
  
  data <- x$get()         # Get the matrix
  inv <- solve(data, ...) # Calculate the inverse
  x$setinverse(inv)       # Cache the inverse
  inv                     # Return a matrix that is the inverse of 'x'
}

