## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## the function 'makeCaheMatrix' contains a list of four objects: set, get, setsolve, and getsolve. 
## 1. set, sets the value of the parameter (matrix) that should be evaluated
##    at function makeCacheMatrix
## 2. get, gets the value of the parameter (matrix). No evaluation at the
##    makeCacheMatrix is made since no new parameter has been entered.
## 3. setsolve, sets the value of the inverse matrix that should be evaluated
##    at function makeCacheMatrix
## 4. getsolve, gets the value of the inverse matrix. No evaluation at the
##    makeCacheMatrix is made since the inverse matrix has already been computed.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
  }
  get <- function() {
    x
  }
  setsolve <- function(matrix.inverse) {
    m <<- matrix.inverse
  }
  getsolve <- function() {
    m
  }
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Write a short comment describing this function
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.
## m is a vector that is set to the element 'getsolve' of the instance 'x' taken from the 
## makeCacheMatrix function.
## when m is not empty it means that the inverse of the matrix has already been stored, and we
## retrieve the computation cached.
## when m is empty it means that the inverse of the matrix has not been stored. A new 
## computation should be made using the element 'get' from the 'x' instance of the  makeCacheMatrix 
## function. The computation of the new matrix is passed to vector m, and then assigned to 
## the element 'setsolve' of the 'x' instance.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

## This is an example
supermatrix <- makeCacheMatrix(matrix(c(1, 8, 9, 10, 15, 12, 3, 4, 5), nrow = 3, ncol = 3))
cacheSolve(supermatrix)

## After asking again for the inverse of the same matrix, we don't make any other computation
## but rather we retrieve the one already cached in memory.
cacheSolve(supermatrix)

