## The two functions in this file cache the inverse of a matrix that is
## useful in the scenario where matrix inversion repeatedly is required.


## makeCacheMatrix returns a list that contains the following functions
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the inverse of the matrix
##  4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns inverse of the input matrix if it is available in
## cache. Otherwise, it inverses the matrix & update the value in the cache
cacheSolve <- function(x, ...) {  
  
  ## get the cache
  cachem <- x$getinverse()
  
  ## returning cached data if it is not null
  if(!is.null(cachem)) {
    message("returning cached data")
    cachem
  ## inverse matrix & update the cache
  } else {
    matrix <- x$get()
    cachem <- solve(matrix, ...)
    x$setinverse(cachem)
    cachem
  }
}