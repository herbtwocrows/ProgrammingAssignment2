## makeCacheMatrix creates a cache for the inverse of a matrix that is 
## passed to it as the argument of the function.
## cacheSolve calculates the inverse of the matrix entered in makeCacheMatrix. 
## It takes the result of makeCacheMatrix as its argument.

## This function creates a cache for the inverse of a matrix. The cache is in the variable m. 
## The function argument is the matrix for which the inverse is desired. 
## The output is a collection of functions.

## To use this function assign it to a variable. This variable contains a vector with
## four functions.: set(),get(), setinverse(), and getinverse().
## set() accepts the new matrix and empties the cache
## get() returns the input matrix.
## setinverse() places the inverse in the cache.
## getinverse() retrieves the inverse from the cache.

makeCacheMatrix <- function(x = numeric()) {
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
             getinverse = getinverse)
}

## This function calculates the inverse of the matrix entered in makeCacheMatrix. 
## To run cacheSolve, the argument is the variable to which you assigned makeCacheMatrix
## The first time you run it, the inverse is returned.
## If the inverse has already been calculated it prints "getting cached data" and 
## the inverse is returned from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}