## Below are two functions that are used to create a 
## special object that stores a matrix
## and cache's its inverse.
## These functions assume the supplied matrix is 
## always invertible.
##
## Usage: Pass makeCacheMatrix an invertible matrix and
##        assign it to a local variable.
##        Then use cacheSolve on the resulting variable
##
## Example : myMatrix <- matrix(rnorm(1:9),3)
##           myCachedMatrix <- makeCacheMatrix(myMatrix)
##           myInvertedMatrix <- cacheSolve(myCachedMatrix)


## makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseX <- NULL
  set <- function(y) {
    x <<- y
    inverseX <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverseX <<- solve
  getinverse <- function() inverseX
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve checks to see if the inverse has 
## already been calculated. If so, it gets the 
## inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data 
## and sets the inverse in the cache via the 
## setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseX <- x$getinverse()
  if(!is.null(inverseX)) {
    message("getting cached data")
    return(inverseX)
  }
  data <- x$get()
  inverseX <- solve(data, ...)
  x$setinverse(inverseX)
  inverseX
}
