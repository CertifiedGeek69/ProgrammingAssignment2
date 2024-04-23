# Function to create a cacheable matrix object
makeCacheMatrix <- function(x = matrix()) {
  # Create a private variable 'j' to store the inverse matrix
  j <- NULL
  # Function to set the matrix value and reset the inverse
  set <- function(y) {
    x <<- y
    j <<- NULL
  }
  # Function to get the matrix value
  get <- function() x
  # Function to set the inverse matrix value
  setInverse <- function(inverse) j <<- inverse
  # Function to get the inverse matrix value
  getInverse <- function() j 
  # Return a list of functions to interact with the cacheable matrix object
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

# Function to calculate the inverse of a matrix, with caching
cacheSolve <- function(x, ...) {
  # Try to retrieve the cached inverse
  j <- x$getInverse()
  # If the cached inverse is available, return it
  if (!is.null(j)) {
    message("getting cached data")
    return(j)
  }
  # If the cached inverse is not available, calculate the inverse
  mat <- x$get()
  j <- solve(mat, ...)
  # Cache the calculated inverse
  x$setInverse(j)
  j
}
