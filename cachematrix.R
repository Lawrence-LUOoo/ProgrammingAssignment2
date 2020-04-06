## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(newMatrix) {
    x <<- newMatrix
    invMatrix <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(inverse) invMatrix <<- inverse
  getInvMatrix <- function() invMatrix
  
  list(
    set = set,
    get = get,
    setInvMatrix = setInvMatrix,
    getInvMatrix = getInvMatrix
  )
}

## Write a short comment describing this function

cacheSolve <-
  function(x, ...) {
    # passing address of makeCacheMatrix as x
    ## Return a matrix that is the inverse of 'x'
    # invMatrix <- x$getInvMatrix only pass function address here
    
    invMatrix <- x$getInvMatrix()
    if (!is.null(invMatrix)) {
      message("getting cached data")
      return(invMatrix)
    }
    mat <- x$get()
    invMatrix <- solve(mat, ...)
    x$setInvMatrix(invMatrix)
    invMatrix
  }
