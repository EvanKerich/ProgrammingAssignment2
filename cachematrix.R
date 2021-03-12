## 03/12/2021

## makeCacheMatrix() - creates a special matrix object. This object is operated
## on by cacheSolve(x).
## cacheSolve(x) - looks to see if the inverse of the special matrix object is
## cached. If the inverse is cached it returns the parameter without calculation.
## If the inverse is not cached it is computed and returned.

## makeCacheMatrix() defines four functions:
##  set - stores the provided matrix to the special matrix object
##  get - returns the stored matrix
##  setinverse - stores an inverse value to the special matrix object
##  getinverse - returns the stored inverse value of the special matrix object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()
    x
  setinverse <- function(inverse)
    inv <<- inverse
  getinverse <- function()
    inv
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
  
}


## cacheSolve uses the defined functions on the special matrix object to:
##  - first check if an inverse is cached
##  - return the value if cached or compute it and store the value to the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
