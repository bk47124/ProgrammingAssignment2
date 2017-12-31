## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

# initiate the inverse property
  my_inv < - NULL 
# set the value of the matix 
  set <- function(matrix) {
    x <<- matrix
    my_inv <<- NULL
    
  }
  
# get the value of the vector 
  get <- function() x
  set_Inv <- function(inverse) my_inv <<- inverse
  get_Inv <- function() my_inv
  list(set = set,
       get = get,
       set_Inv = set_Inv,
       get_Inv = get_Inv)
}


### This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  my_inv <- x$get_Inv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(my.inv)
  }
  mat <- x$get()
  my_inv <- solve(mat, ...)
  x$set_Inv(my_inv)
  my_inv
}




