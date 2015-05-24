## These functions simplify the process of computing the
## inverse of a matrix, by caching its value once it has
## been computed


## This function creates a container object for a matrix and
## its inverse, represented as a list of two get-set pairs

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


## This function calculates the inverse of a container 
## object created with the above function, retrieving
## a cached copy if one exists

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
