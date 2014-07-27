## makeCacheMatrix() and cacheSolve() work together to cache a matrix and its
## inverse and also defining functions to perform those operations and
## enable retrival of the matrix and its inverse matrix

## Typical usage would be to assigning the results of makeCacheMatrix() to a
## variable (e.g. a <- makeCacheMatrix()). A numeric matrix can be passed as an
## argument initially (e.g. a <- makeCacheMatrix(matrix) or set/redifined
## later (e.g. a$set(matrix))
## cacheSolve() can then be called to calculate the inverse (e.g. cacheSolve(a))
## The inverse matrix will then be available by getInverse() 
## (e.g. a$getInverse())

## makeCacheMatrix(matrix) takes a numeric matrix or empty argument
## returns a list of functions:
##      get() - returns the matrix
##      set(matrix) - set or redefine the matrix
##      setInverse(matrix) - sets the matrix inverse. For use by cacheSolve function
##      getInverse() - returns the inverse matrix

makeCacheMatrix <- function(matrix = numeric()) {
    # initialize inverse to NULL as a flag that inverse not calculated yet
    inverse <- NULL

    # if set() is used to (re)define the matrix, inverse is reset to NULL flag 
    set <- function(y) {
        matrix <<- y
        inverse <<- NULL
    }

    # setInverse() is used to cache the inverse matrix
    # for use by cacheSolve() shouldn't be called by user
    setInverse <- function(inv) inverse <<- inv
    
    # get(), getInverse() simply return the matrix, and inverse respectfully
    get <- function() matrix
    getInverse <- function() inverse

    # return a list of the functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve(matrix,...) takes a makeCacheMatrix() argument and optional arguments
## for use by the solve() function
## returns the inverse of the matrix and also caches the inverse in the
## setInverse() function of makeCacheMatrix

cacheSolve <- function(madematrix, ...) {
    # check if inverse has already been calculated and cached. If so simply return it.
    inverse <- madematrix$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }

    # extract matrix from argument and use solve() to find inverse
    matrix <- madematrix$get()
    inverse <- solve(matrix, ...)

    # cache the calculated inverse then also return it
    madematrix$setInverse(inverse)
    inverse
}
