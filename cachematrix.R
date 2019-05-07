## This first function creates a matrix object that obtain the mode
## as a result

makeCacheMatrix <- function(x = matrix()) {
t <- NULL
set <- function(y) {
    
    x <<- y
    t <<- NULL
}
get <- function() x
setMode <- function(mode) t <<- mode
getMode <- function() t
list(set = set,
       get = get,
       setMode = setMode,
       getMode = getMode)
}

## This second function computes the mode of the matrix created by 
## makeCacheMatrix above. If the mode has already been calculated 
## then it should retrieve the mode from the cache via the setmean 
## function.


cacheSolve <- function(x, ...) {
t <- x$getInverse()
  if (!is.null(t)) {
    message("getting cached data")
    return(t)
  }
  data <- x$get()
  t <- mode(data, ...)
  x$setMode(t)
  t
}
