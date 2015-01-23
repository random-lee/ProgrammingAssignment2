## R Programming Assignment 2 - Caching the Inverse of a Matrix

## This function make a list of functions to get a matrix and cache its inverse 

makeCacheMatrix <- function(x = matrix()) {

  inv_m <- NULL # initializes the inverse to NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x # Getting the starting matrix
  setInverse <- function(inverse_m) inv_m <<- inverse_m # Setting the inverse matrix
  getInverse <- function() inv_m # Getting the inverse matrix
  
  # List of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


# Returns cahced inverse matrix and calculates the inverse matrix if one has not already been cached

cacheSolve <- function(x, ...) {
  
  inv_m <- x$getInverse() # Calls the cached inverse matrix
  
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m) # Returns the cached inverse matrix if is not NULL
  }
  
  m <- x$get() # Getting the starting matrix 
  inv_m <- solve(m, ...) # Calculates the inverse matrix
  x$setInverse(inv_m) # Caches the inverse matrix
  inv_m # Displays the inverse matrix
}


# Test:
# 
# > test_mat <-rbind (c(4,7), c(2,6))
# > testX <- makeCacheMatrix(test_mat)
# > testX$get()
#       [,1] [,2]
# [1,]    4    7
# [2,]    2    6
# > testX$getInverse()
# NULL
# > cacheSolve(testX)
#       [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
# > testX$getInverse()
#       [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
