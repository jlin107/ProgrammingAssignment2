## A pair of functions that cache the inverse of a matrix

## Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Inverse of the matrix
  inv <- NULL
  
  # 1. set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #2. get the value of the matrix
  get <- function() x
  #3. set the value of the inverse
  setinverse <- function(inverse) inv <<- inverse
  #4. get the value of the inverse
  getinverse <- function() inv
  
  # List containing a function to 1-4
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Computes the inverse of the special matrix returned by makeCacheMatrix 
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  ## If the inverse has already been calculated, 
  ## retrieve the inverse from cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## If the inverse has not already been calculated,
  ## calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
