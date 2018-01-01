## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  my_inv <- NULL
#1. set the value of the matrix
  set <- function(y) {
    x <<- y
    my_inv <<- NULL
  }
#2. get the value of the matrix
  get <- function() x
#3. set the value of the inverse
  set_Inverse <- function(inverse) my_inv <<- inverse
#4. get the value of the inverse
  get_Inverse <- function() my_inv
  list(set = set,
       get = get,
       set_Inverse = set_Inverse,
       get_Inverse = get_Inverse)
}




cacheSolve <- function(x, ...) {
# Return a matrix that's the inverse of 'x'
  my_inv <- x$get_Inverse()

# 
  if (!is.null(my_inv)) {
    message("getting cached data")
    return(my_inv)
  }
  my_mat <- x$get()
  my_inv <- solve(my_mat, ...)
  x$set_Inverse(my_inv)
  my_inv
}