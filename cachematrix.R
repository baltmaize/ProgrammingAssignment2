## Code here creates a special matrix whose inverse can be cached 

## Function 'makeCacheMatrix' creates a special matrix whose inverse
## can be cached. Function returns a list of available functions:
## - 'setMatrix' -> set new value to the matrix. This also clears cached value.
## - 'getMatrix' -> returns the actual matrix
## - 'setInverse' -> sets the inverse matrix to be cached
## - 'getInverse' -> returns the cached value

makeCacheMatrix <- function(matrix = matrix()) {
    inverse <- NULL;
    
    setInverse <- function(inverse){
        inverse <<- inverse;
    }
    
    getInverse <- function(){
        inverse;
    }
    
    getMatrix <- function(){
        matrix;
    }
    
    setMatrix <- function(matrix){
        matrix <<- matrix;
        inverse <<- NULL;
    }
    
    list(getMatrix = getMatrix, setMatrix = setMatrix,
         setInverse = setInverse, getInverse = getInverse);
}


## Function 'cacheSolve' returns a matrix that is the inverse of 'cacheMatrix'
## created by function 'makeCacheMatrix'.
## If the inverse of given matrix is not calculated before, it will calculate
## the inverse and save the value in cache. 
## Subsequent calls to this function with the same matrix will
## return the cached result.

cacheSolve <- function(cacheMatrix, ...) {
    
    inverse <- cacheMatrix$getInverse();
    if(!is.null(inverse)){
        return(inverse);
    }
    matrix <- cacheMatrix$getMatrix();
    inverse <- solve(matrix, ...);
    cacheMatrix$setInverse(inverse);
    inverse;
}
