## Functions below can be used to cache the inverse of a matrix
## The function makeCacheMatrix creates a special matrix with 
## getter and setters for the matrix and matrix inverse
## The function cacheSolve uses the special matrix. It first
## checks if the matrix inverse is already set (cached).
## If set (cached) then it is returned, if not then the matrix inverse
## is computed, set (cached) and then returned

## Function return a list of 4 functions:
## setMatrix   - To set the matrix
## getMatrix   - To get the matrix
## setInverse  - To set the inverse matrix
## getInverse  - To get the inverse matrix
makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    setMatrix <- function(y) {
        mat <<- y
        inv <<- NULL
    }
    getMatrix <- function() mat
    setInverse <- function(inverse) inv <<- inversecac
    getInverse <- function() inv
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Function return the inverse matrix of the provided matrix x
## If the inverse of x is already cached it will be returned immediately
## If not then it is caluclated, cached and returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$getMatrix()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
