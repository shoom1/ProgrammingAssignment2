## This file contains the implementation of cached matrix inverse
## In order ot be cached, the matrix should be wrapped in the special matrix object
## via function call:  
##   mCached<- makeCacheMatrix(m)
## where m is the matrix to be cached
## The the inverser calculation of the matrix object will be prformed only once:
##   cacheSolve(mCached)   <- First time perfomes actual inversion
##   cacheSolve(mCached)   <- Second time simply returns the previously calculated inverse matrix
##   

## This function creates a special matrix object that contains a list of functions:
## * get() to get the value of the matrix
## * set(x) to set the value of the matrix
## * setinv(inv) to set the value of the inverse matrix
## * getinv() to get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invrs) inv <<- invrs
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cashSolve(x, ...) function calculates the inverse of the matrix 
## that is stored in the special matrix object created by the function makeCashMatrix
## It first checks if the inverse is already calculated adn returns it in this case. 
## If not the inverse is not available, then it calculates the inverse, 
## stores it in the special matrix object and returns it.
## 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}