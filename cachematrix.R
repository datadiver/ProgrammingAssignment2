## Curse 'R PROGRAMMING': Programming Assignament 2
## Juan José Garcés Iniesta

## This function creates a special "matrix" object that can cache 
## its inverse:

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinv <- function(inv) I <<- inv
  getinv <- function() I
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function computes the inverse of 'x' that is a special "matrix" 
## constructed using `makeCacheMatrix`

cacheSolve <- function(x, ...) {
    
  ## Taking data from cache. 'I' will be NULL if the inverse has not 
  ## already calculated and it will be the inverse matrix otherwise
  I <- x$getinv() 
  
  ## If the inverse has already been calculated (and the matrix has not changed),
  ## then `cacheSolve` should retrieve the inverse from the cache.
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  ## If the inverse has not been calculated, we have to do it: 
  message("calculating inverse matrix")
  data <- x$get()
  I <- solve(data, ...)
  ## And store the resulting matrix in cache for future uses
  x$setinv(I)
  
  ## Returning 'I' as results of the function: 
  I
}
