## cacheSolve checks it the inverse of the matrix provided as input already exist in cache. If it exists
## then the inverse is returned without relcalculating it. 
## Otherwise cacheSolve computes the inverse and stores the new matrix and inverse. 

## makeCacheMatrix function stores the matrix and its inverse. 
## It also provides the get/set functions for accessing the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    matrixObjectInverse <- NULL
    
    setmatrixObject <- function(y) {
        x <<- y
    }
    getmatrixObject <- function() x
    setmatrixInverse <- function(object) {
        matrixObjectInverse <<- object
    }
    getmatrixInverse <- function() matrixObjectInverse
    
    list(setmatrixObject = setmatrixObject, getmatrixObject = getmatrixObject,
         setmatrixInverse = setmatrixInverse,
         getmatrixInverse = getmatrixInverse)
}


## cacheSolve access the matrix to check if it is the same matrix which was earlier provided. 
## It then computes inverse if inverse is not already calculcated. Otherwise the inverse is returned
## from cache. For a new matrix, inverse is always calculated.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Check if Matrices are same. If same check if inverse exists.
    if(identical(x$getmatrixObject(), as.matrix(...))){
        # Inverse is not calculated.
        if(is.null(x$getmatrixInverse())){
            x$setmatrixInverse(solve(as.matrix(...)))
        }
    } else {
        
        # Matrices are not same. Store the new matrix and calculate and store inverse.
        x$setmatrixInverse(solve(as.matrix(...)))
        x$setmatrixObject(as.matrix(...))
    }
    
    x$getmatrixInverse()
    
}
