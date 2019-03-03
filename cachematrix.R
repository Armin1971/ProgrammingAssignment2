## Put comments here that give an overall description of what your
## functions do
## Function to fulfill Week 3 Assignment,
## of Coursera Data Science, R Programming
## 04.03.2019

# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
# rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here).
# Your assignment is to write a pair of functions that cache the inverse of a matrix.
#
# Write the following functions:
#
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## Write a short comment describing this function

# makeVector <- function(x = numeric()) {
#   m <- NULL
#   set <- function(y) {
#     x <<- y
#     m <<- NULL
#   }
#   get <- function() x
#   setmean <- function(mean) m <<- mean
#   getmean <- function() m
#   list(set = set, get = get,
#        setmean = setmean,
#        getmean = getmean)
# }

makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

  makeCacheMatrix <- function(x = matrix())   # creates a special "matrix" object
    MatrixInv <- NULL                         # initialize MatrixInv to NULL, MatrixInv contains the matrix inverse
    set <- function(y) {                      # defines the set function
      x <<- y                                 # to set value of the maxtrix
      MatrixInv <<- NULL                      # and initialize inverse to NULL
    }
    get <- function() x                       # get function returns value of x (the new matrix object)

    setMatrixInverse <- function(MatrixInverse) MatrixInv <<- MatrixInverse  # assigns value of MatrixInverse to MatrixInv
    getMatrixInverse <- function() MatrixInv                                 # gets value of MatrixInv
    list(set = set, get = get,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)
}


## Write a short comment describing this function
# cachemean <- function(x, ...) {
#   m <- x$getmean()
#   if(!is.null(m)) {
#     message("getting cached data")
#     return(m)
#   }
#   data <- x$get()
#   m <- mean(data, ...)
#   x$setmean(m)
#   m
# }

cacheSolve <- function(x, ...) {
  ## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
  MatrixInv <- x$getMatrixInverse()
  if(!is.null(MatrixInv)) {
    message("getting cached data")
    return(MatrixInv)
  }
  data <- x$get()
  MatrixInv <- solve(data, ...)
  x$setMatrixInverse(MatrixInv)
  MatrixInv
}
