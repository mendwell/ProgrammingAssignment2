## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix instead of computing it repeatedly.

## This creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        make <- function(y){
                x <<- y
                inv <<- NULL
        }
        find <-function()x
        makeInverse <- function(inverse) inv <<- inverse
        findInverse <- function()inv
        list (make=make,
              find=find,
              makeInverse = makeInverse,
              findInverse = findInverse)
}


## This function computes the inverse of the matrix created by the function above.
## if the inverse has already been calculated then it should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$findInverse()
        if (is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$find()
        inv <- solve(mat, ...)
        x$makeInverse(inv)
        inv
}
