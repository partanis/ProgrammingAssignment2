## This function stores the matrix and inverse of matrix in the session. The inverse of matrix is calculated once
## only, on all the subsequent calls the functions returns the cached value of inverse.
## There are some boiler plate validations of the matrix such as if it is the matrix class or if it is a square matrix

## Validates: If the class is matrix class, also if matrix is square/invertible
## set, get matrix, setinv and getinv 
makeCacheMatrix <- function(x = matrix()) {
  ## Check for matrix
  if (!is.matrix(x)) {
    message("This is not a matrix") 
    return()
  }
  ## Check if the matrix is square or invertible
  dx <- dim(x)
  if (dx[1] != dx[2]) {
    message("This is not a square matrix. Dim", dx[1], "x", dx[2])
    return()
  }
  matrx <- x
  invmatrx <- NULL
  set <- function(y) {
    matrx <<- y
  }
  get <- function() matrx
  setinv <- function(invmat) {
    invmatrx <<- invmat
  }
  getinv <- function() invmatrx
  list ( set = set, get = get, getinv = getinv, setinv = setinv)
}

## If the inverse is already calculated, return it from cache. 
## Else compute the inverse using solve and set it in the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmatrx <- x$getinv()
  if (!is.null(invmatrx)) {
    messsage("getting the cached inverse")
    return(invmatrx)
  }
  invmatrx <- solve(x$get())
  x$setinv(invmatrx)
  invmatrx
}
