## Matrix inversion is usually a costly computation
## These fuctions allow caching the inverse of a matrix
## rather than compute it repeatedly

## This function creates a special "matrix" object that can cache
## its inverse. This object contains 4 methods
## (get, set, getinverse, setinverse)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## get the matrix
    get <- function() x
    
    ## get the cached inverse or null if its not avalaible
    setinverse <- function(inverse) inv <<- inverse
    
    ## save the inverse in the cache
    getinverse <- function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    ## return cached data if possible
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## else calculate inverse and save in the cache
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
