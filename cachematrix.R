## This program computes inverse of the given matrix. 
## Also checks if it can be retrieved from the cache.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinve <- function(inve) inv <<- inve
  getinve <- function() inv
  list(set=set, get=get, setinve=setinve, getinve=getinve)
}


## The following function calculates the inverse of the matrix created with the above 
## function. However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinve function.


cacheSolve <- function(x, ...) {
  inve <- x$getinve()
  if(!is.null(inve)) {
    message("getting the cached data")
    return(inve)
  }
  data <- x$get()
  inve <- solve(data)
  x$setinve(inve)
  inve
}

## Sample Run
# > x = rbind(c(1, -1/2), c(-1/2, 1))
# > m = makeCacheMatrix(x)
# > m$get()
# [,1] [,2]
# [1,]  1.0 -0.5
# [2,] -0.5  1.0
# > cacheSolve(m)
# [,1]      [,2]
# [1,] 1.3333333 0.6666667
# [2,] 0.6666667 1.3333333
# > cacheSolve(m)
# getting the cached data
# [,1]      [,2]
# [1,] 1.3333333 0.6666667
# [2,] 0.6666667 1.3333333
