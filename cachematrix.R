## These functions create a special matrix object that can be reversed, and can cache that reversal. The second function is where the reversal is actually done.

## This creates a special matrix. It sets the value of the matrix, gets the value of the matrix, sets the value of the inverse matrix, and gets the value of the inverse matrix.==

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
          x <<- y
          m <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(solve) m <<- solve
  getinvmatrix <- function() m
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}

## The following function calculates the inverse of the matrix created with the above function. However, it first checks to see if the inverse matrix has already been calculated. If so, it gets that from the cache and skips the computation. Otherwise, it calculates the inverse matrix and sets the value in the cache via the setinvmatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvmatrix(m)
        m
}