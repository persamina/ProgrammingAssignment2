## Functions to cache and calculate the inverse of a matrix.

## Caches the matrix and the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(newMatrix) {
    x <<- newMatrix
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverseMatrix <<- inverse
  getInverse <- function() inverseMatrix

  list(set = set, 
       get = get, 
       getInverse = getInverse, 
       setInverse = setInverse)
}


## Returns the inverse of a matrix. Solves the inverse of the matrix only if it
## hasn't been solved yet.
cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  matrixToInverse <- x$get()
  inverseMatrix <- solve(matrixToInverse, ...)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
