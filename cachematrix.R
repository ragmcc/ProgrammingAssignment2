## This is a special Matrix object, that can 
## hold as cache result values of complex 
## operations run on it. 

## Constructor function of the Matrix
makeCacheMatrix <- function(x = matrix()) {
    # Local value that hold the Inverse Matrix
    inverse <- NULL
    
    # Update value of local Matrix and reset cache.
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    # Allow retrieve local matrix
    get <- function() x
    
    # Update inverse value
    setinverse <- function(inv) inverse <<- inv
    # retrieve inverse matrix value
    getinverse <- function() inverse
    
    # return new cMatrix as a list of functions. 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function responsible run Solve operation on 
## a CacheMatrix object and update cache.

cacheSolve <- function(x, ...) {
    # check current value
    inverse <- x$getinverse()
    # if not null use cache 
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    # if no cache exist, run the operation
    data <- x$get()
    inverse <- solve(data, ...)
    # update cache value
    x$setinverse(inverse)
    ## Return a matrix that is the inverse of 'x'
    inverse    
}
