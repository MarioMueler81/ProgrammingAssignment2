## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ##The first function, makeCacheMatrix creates a special "maxtrix", 
  ##which is really a list containing a function to
  ##      1. set the value of the vector
  ##      2. get the value of the vector
  ##      3. set the value of the invrs
  ##      4. get the value of the inverse
  inv = NULL
  set <- function(y) {
    
    ##<<- operator which can be used to assign a value to an 
    ##object in an environment that is different from the current
    ##environment. Below are two functions that are used to create 
    ##a special object that stores a numeric vector and cache's its 
    ##mean.
    x <<- y
    inv <<- NULL
  }
  get <- function() x 
  setinv <- function(inv) inv <<- inv
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <-  x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    mat.data <- x$get()
    inv <- solve(mat.data, ...)
    x$setinv(inv)
    return(inv)
  }

