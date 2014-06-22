## Memorize the solve() function for matrices by building an object with
## makeCacheMatrix and using the object with cacheSolve

## return a list of functions to get and set a matrix
## and 2 functions to get and set an inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Use the list created from makeCacheMatrix and check for a cached value
## if not found calculate it and set the value
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached value")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
