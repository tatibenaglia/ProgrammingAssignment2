## The two functions below can be used to cache the inverse of a matrix.

## The first function 'makeCacheMatrix' creates a special "matrix", which is 
## really a list containing four functions:
## 1) set: set the value of the matrix
## 2) get: get the value of the matrix
## 3) setinv: set the value of the inverse of the matrix
## 4) getinv: get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL
    set <- function(y) {
        x <<- y
        invmat <<- NULL
    }
    get <- function() x
    setinv <- function(inv) invmat <<- inv
    getinv <- function() invmat
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The second function 'cacheSolve' calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the matrix and 
## sets the value of the inverse in the cache via the 'setinv function.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    invmat <- x$getinv()
    if(!is.null(invmat)) {
        message("getting cached data")
        return(invmat)
    }
    mat <- x$get()
    invmat <- solve(mat, ...)
    x$setinv(invmat)
    invmat
}
