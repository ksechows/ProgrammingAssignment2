## The functions below will be used to cache potentially time-consuming
## computations.  In this case, the inverse matrix will be computed and then cached.
## If the same matrix is then used, the cached inversion will be used rather
## than recalculated.

## The makeCacheMatrix() function will create a matrix and cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m < NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(solve) m <<- solve
  getinvmatrix <- function() m
  list (set = set, get = get,
        setinvmatrix = setinvmatrix,
        getinvmatrix = getinvmatrix)
}


## the cacheSolve() function will accept a matrix parameter.  If the matrix inversion is found in the cache, it will
## be used, otherwise it will recalculate the inversed matrix

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinvmatrix()
        if(!is.null(m)){
          message("getting data from cache")
          return(m)
        }
        invmatrix <- x$get()
        m <- solve(invmatrix, ...)
        x$setinvmatrix(m)
        m
}
