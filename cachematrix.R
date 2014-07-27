## makeCacheMatrix creates a cache matrix
## contains getter and setter for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  get <- function() x
  setInvM <- function(inv) invMat <<- inv
  getInvM <- function() invMat
  list(set = set, get = get,
       setInvM = setInvM,
       getInvM = getInvM)
}


## cacheSolve gets the inverse of the matrix x from the cache
## if not present in cache, it creates the inverse of x and stores
## it in cache for re-use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMat <- x$getInvM()
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  data <- x$get()
  invMat <- solve(data, ...)
  x$setInvM(invMat)
  invMat
}
