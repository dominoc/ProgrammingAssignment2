## Put comments here that give an overall description of what your
## functions do
##
## Two functions here (1) makeCacheMatrix and (2) cacheSolve help
## to compute the inverse of a matrix by using a cache to reduce
## the computation needed for a repeated matrix

## Write a short comment describing this function
## 
## makeCacheMatrix creates a special "object" or list with functions
## to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
##
## Some examples to create a matrix for testing:
## m <- matrix ( c(2, 3,   2, 2), nrow = 2, ncol = 2, byrow = TRUE)
## m <- matrix(c(3,7,0,4), nrow = 2, ncol = 2)
##

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function () x
  
  setinverse <- function (inverse) inv <<- inverse
  
  getinverse <- function () inv
  
  list ( set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## Write a short comment describing this function
##
## cacheSolve is a function that calculates the inverse
## of a matrix cached using the makeCacheMatrix "object"
## 

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return (inv)
  }
  
  ## No cached matrix, go and 
  ## solve for it
  the_matrix <- x$get()
  
  inv <- solve(the_matrix, ...)
  x$setinverse(inv)
  inv
}
