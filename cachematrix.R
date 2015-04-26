## cachematrix.R
## Solve the inverse of a matrix using caching for speed. 
## 

## Creates a special "matrix" of four functions to allow caching of 
## the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## Set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Get the matrix
  get <- function() x
  
  ## Set the inverse of the matrix
  setinverse <- function(solve) m <<- solve
  
  ## Get the inverse of the matrix
  getinverse <- function() m
  
  ## Return a list of available methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Solve the inverse of a matrix unless it's cached
cacheSolve <- function(x, ...) {
  
  ## Get the cached inverse if available
  m <- x$getinverse()
  
  ## If we do have a cached inverse
  if(!is.null(m)) {
    ## We're using cached data, yay!
    message("getting cached data")
    ## Return the cached inverse matrix
    return(m)
  }
  
  ## Get the matrix data
  data <- x$get()
  ## Save the inverse
  m <- solve(data)
  ##Cache it
  x$setinverse(m)
  ##Return it
  m
}
