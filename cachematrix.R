## Put comments here that give an overall description of what your
## functions do

## creates a list of function to set and get matrix, and to set and get inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse
  )
}


## Write a short comment describing this function
# This function gets cached data if m is not null and notifies if
# that is the case
# otherwise it runs a solve and assigns to m to keep track for future runs

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
