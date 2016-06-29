## These functions, when used together as cacheSolve(makeCacheMatrix(x))
## where x is a square invertible matrix, return the inverse of x.

## The makeCacheMatrix function creates a list from the matrix input.
## It sets up the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(yayCoursera) m <<- yayCoursera
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## The cacheSolve function takes the input from the makeCacheMatrix function
## and returns the inverse of x using solve().

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
