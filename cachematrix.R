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

