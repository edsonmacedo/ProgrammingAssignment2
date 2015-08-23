## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse
## of a matrix rather than compute it repeatedly. 

## This file presents a pair of functions that
## cache the inverse of a matrix.

## The first function makeCacheMatrix creates
## a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y = matrix()) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inv <<- solve
    getsolve <- function() inv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## The second function cacheSolve computes the inverse
## of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated
## (and the matrix has not changed), then the function
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getsolve()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setsolve(inv)
    inv
}