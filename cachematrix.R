## Function makeCacheMatrix creates a special matrix object that can cache its inverse to save
## on processing time.

makeCacheMatrix <- function(x = matrix()) {  
  inv = NULL
  
  ## set the matrix
  set = function(y) {
    ## use `<<-` to assign a value to an object in an environment different from the current one. 
    x <<- y
    inv <<- NULL
  }
  ## get the matrix
  get = function() x
  ## set the inverse
  setinv = function(inverse) inv <<- inverse 
  ## get the inverse
  getinv = function() inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Function cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix did not change then it will retrieve
## from cache.

cacheSolve <- function(x, ...) {
  
  inv = x$getinv()
  
  ## if the inverse has already been calculated retrieve it from cache
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  ## if not previously calculated, then perform the calculation 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # set the value of the inverse in the cache.
  x$setinv(inv)
  
  return(inv)
}
