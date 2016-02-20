## A pair of functions that cache the inverse of a matrix
## Assumes that the matrix passed in is square and has an inverse
## Example Usage:  myM <- makeCacheMatrix(matrix(c(1,2,3,4), nrow = 2, ncol = 2))
##                 cacheSolve(myM)

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
            i <- NULL
            set <- function(y) {
                    x <<- y
                    i <<- NULL
            }
            get <- function() x
            setinv <- function(inv) i <<- inv
            getinv <- function() i
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.
## If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve`
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
            i <- x$getinv()
            if(!is.null(i)) {
                    message("getting cached data")
                    return(i)
            }
            data <- x$get()
            i <- solve(data, ...)
            x$setinv(i)
            i
}
