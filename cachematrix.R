## The following functions allow a user to cache the inverse of a matrix, rather than re-compute
## every time the inverse is needed

## makeCacheMatrix returns a list of four functions, and creates the environment
## where the inverse of the matrix is stored. The user first sets the inverible matrix 
## by assigning makeCacheMatrix() to a name in the console, then calling name$set with
## the invertible matrix as the argument. After calling cacheSolve on the name, 
## the inverse will be cached in inv. Re-calling name$set will clear the cache for a 
## new computation.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
     
}

## cacheSolve uses the other three functions in the list created by the above
## to compute the new inverse and cache it, or recall the cached inverse as desired.

cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv)
     inv
}
