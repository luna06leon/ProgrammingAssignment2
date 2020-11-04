## The functions here are useful to make matrix's inverse more easy, even more when we need 
##to calculate inverse several times, so these functions can cache the inverse
##so we can use in the future without recalculate it


## "makeCacheMatrix" is the function that can cache the inverse of an input matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(r=x) {
    y <<- r
    m <<- NULL
  }
  get <- function() x
  setinvs <- function(solve) m <<- solve
  getinvs <- function() m
  list(set = set, get = get,
       setinvs = setinvs,
       getinvs= getinvs)
}


## "cacheSolve" is the function in charge of calculate the inverse of the input matrix if this 
## if this has not been calculated before, otherwise the function will take the inverse from the cache
## and return the value

cacheSolve <- function(x, ...) {
  m <- x$getinvs()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvs(m)
  m
}
