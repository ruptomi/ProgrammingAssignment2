## The Function for creating inverted matrices which caching ability

## Creates cacheable matrix for inputting to cacheSolve()
## function which sets and gets the cached values

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y)
    {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Computes the inverse of the cacheable matrix

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
