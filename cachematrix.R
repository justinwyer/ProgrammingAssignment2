## create a cacheable matrix and cache the inverse of the matrix using google's
## R code style, except for the function names the function names would be
## MakeCacheMatrix & CacheSolve but that would break the assignment's contract
## I would have chosen MakeCachableMatrix & SolveCachableMatrix

## accept or create a new matrix and add functions to get or set the inverse
makeCacheMatrix <- function(unwrappedMatrix = matrix()) {
  inverseMatrix <- NULL
  ## set the matrix and clear the cache
  Set <- function(newMatrix) {
    unwrappedMatrix <<- newMatrix
    inverseMatrix <<- NULL
  }
  Get <- function() unwrappedMatrix
  SetInverse <- function(newInverseMatrix) inverseMatrix <<- newInverseMatrix
  GetInverse <- function() inverseMatrix
  return(list(Set = Set, Get = Get,
       SetInverse = SetInverse,
       GetInverse = GetInverse))
}


## Return the cached inverse or solve, cache and return the inverse
cacheSolve <- function(cachedMatrix) {
  inverseMatrix <- cachedMatrix$GetInverse()
  if(!is.null(inverseMatrix)) {
    message("using cache")
    return(inverseMatrix)
  } else {
    unwrappedMatrix <- cachedMatrix$Get()
    inverseMatrix <- solve(unwrappedMatrix)
    cachedMatrix$SetInverse(inverseMatrix)
    return(inverseMatrix)
  }
}
