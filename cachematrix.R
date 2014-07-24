## makeCacheMatrix
## if there is no change in matrix, inverse is retrieved from cache.

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL ## the cached inverse matrix
  
  ## if matrix is changed with set function, invV is reset to NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  ## get matrix 
  get <- function() x
  
  ## set inverse function
  setinverse <- function(inv) invM <<- inv
  
  ## get inverse function
  getinverse <- function() invM
  
  ## using list as interface to access function
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## if inverse matrix does not exist in cache variable, calculate an inverse matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinverse(inv)
  return(inv)
}
