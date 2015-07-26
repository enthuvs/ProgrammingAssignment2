## Suresh Kumar       Dated: 26/07/2015
## The following functions cache and compute the inverse of a matrix.

## The function makeCacheMatrix creates a special object: "matrix", that can cache its inverse.
##Usage example:
## > source('cachematrix.R')
## > mat <- makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2)))
## > cacheSolve(mat)
##      [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}
## Calculate the inverse of the special "matrix" created with makeCacheMatrix function, 
## reusing cached result if it is available.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
i <- x$getinverse()
if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}
