## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  ## Initialize the cached function
  cached <- NULL
  
  ## set the matrix
  set <- function(y) {
    x <<- y
    cached <<- NULL
  }
  
  ## get the matrix
  get <- function() x
  
  ## set and get the inverse of the matrix
  setinverse <- function(inverse) cached <<- inverse
  getinverse <- function() cached
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Get inverse
  inverse <- x$getInverse()
  
  ## If the inverse is not null, we print the message
  if(!is.null(inverse)) {
    message("getting cached data")
  }
  else {
    ## if it's not set, we get and set it
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
  }
  
  ## Finally we return the inverse
  inverse
}
