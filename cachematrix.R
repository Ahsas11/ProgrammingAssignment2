## Solve for the inverse of a matrix and cache the outcome
## Reducing the need for repeated solving of the same matrix

## Creates a matrix with functions to store the inverse of the matrix
## Saves from having to repeatedly solve for the inverse when called for

makeCacheMatrix <- function(x = matrix()) {
  sM <- NULL
  set <- function(y) {
    x <<- y
    sM <<- NULL
  }
  get <- function() x
  setSolvedMatrix <- function(solve) sM <<- solve
  getSolvedMatrix <- function() sM
  list(set = set, get = get,
       setSolved = setSolvedMatrix,
       getSolved = getSolvedMatrix)
}


## Calculates the inverse of a matrix and caches the result

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getSolved()
  if(!isnull(sM)) {
    message("retrieving cached value")
    return(sM)
  }
  data <- x$get()
  sM <- solve(data, ...)
  x$setSolved(sM)
  sM
}