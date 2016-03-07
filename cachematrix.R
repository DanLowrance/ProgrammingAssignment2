## Put comments here that give an overall description of what your
## functions do

## Function to establish special matrix for caching the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    }
  get <- function() x
  setMatrix <- function(solve) m <<- solve
  getMatrix <- function() m
  list(set = set, get = get,
         setMatrix = setMatrix,
         getMatrix = getMatrix)
}


## Check to see if inverse exists then a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrix(m)
  m
}
