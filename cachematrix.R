## Functions to cache the inverse of a matrix to avoid computing it repeatedly. 

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        t <- NULL
        set <- function(y) {
                x <<- y
                t <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) t <<- solve
        getsolve <- function() t
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        t <- x$getsolve()
        if(!is.null(t)) {
                message("getting cached data")
                return(t)
        }
        data <- x$get()
        t <- solve(data, ...)
        x$setsolve(t)
        t
}
