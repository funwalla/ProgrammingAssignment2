## makeCacheMatrix creates a list object that encapsulates an input
## matrix and provides get and set functions for the matrix and its
## inverse.

## cacheSolve takes a list object created by makeCacheMatrix. If the
## input object contains a non-null inverse, it is returned. If not,
## the inverse is computed, stored in the input object, and returned.
## Note: cacheSolve assumes the matrix stored in the input object is
## invertible.

## makeCacheMatrix
## input:  a standard R matrix
## output: a list of get and set functions for the matrix:
##         get():  returns the input matrix
##         set(m): sets the stored matrix to the matrix m
##         getinverse():    returns the stored inverse matrix or
##                          NULL if no inverse has been stored
##         setinverse(inv): sets the stored inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    set <- function(m) {
        x   <<- m
        inv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(minverse) {
        inv <<- minverse
    }
    
    getinverse <- function () inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## cacheSolve
## input:  a list object created by makeCacheMatrix.
## output: the cached inverse matrix. If no inverse has been cached,
##         the inverse is computed, cached in the input object, and
##         returned.

cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()
    
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    inv <- solve(x$get())
    
    x$setinverse(inv)
    
    inv
}
