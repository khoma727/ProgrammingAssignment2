## makeCacheMatrix creates a special matrix object that caches its 
##  inverse
## cacheSolve computes the inverse of the matrix object created by 
##  makeCacheMatrix
## if the inverse of the matrix has been calculated, cacheSolve will
##  retrieve the inverse value from the cache


## creates special matrix object to calculate inverse

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL
    set <- function (y) {
    # new operator, use <<- to assign a value to an object in an 
    # environment that's different from the current environment
    
    x <<- y
    inv <<- NULL
  }
    
    get <- function() x
    setinv <- function(inverse)  inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## check special matrix from above and cache inverse or calculate
##  new inverse

cacheSolve <- function(x, ...) {
       
    inv <- x$getinv()
    
    # if inverse is already calculated
    
    if (!is.null(inv)) {
      # use cache instead of re-computing
      
      message("getting cached data")
      return (inv)
    }
    
    # if not, calculate inverse of the matrix
    
    data <- x$get()
    inv <- solve(data, ...)
    
    # use setinv to set value of inverse in cache
    
    x$setinv(inv)
    
    return(inv)
}
