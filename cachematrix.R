## makeCacheMatrix() and cacheSolve() functions are used to caching the inverse of a given matrix.

## SAMPLE RUN
# > matrix <- rbind(c(1, -1/4), c(-1/4, 1))
# > m = makeCacheMatrix(matrix)
# > m$getMatrix()
# > cacheSolve(m)
## Second time, will get cached data (:
# > cacheSolve(m) 

## makeCacheMatrix() returns a list of getters and setters:
## getMatrix and setMatrix: get and set the value of the matrix.
## getInverse and setInverse: get and set the inverse value of the matrix.

makeCacheMatrix <- function(m = matrix()) {
  
  ## Initialize the inverse of the matrix  
  inverse_m <- NULL
  
  ## Set the matrix
  setMatrix <- function(matrix) {
    m <<- matrix
    inverse_m <<- NULL
  }
  
  ## Get the matrix
  getMatrix <- function() m
  
  ## Set the inverse of the matrix
  setInverse<- function(inverse) inverse_m <<- inverse
    
  ## Get the inverse of the matrix
  getInverse <- function() inverse_m
  
  ## Return list of arguments
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
  
}
  
## cacheSolve() returns the inverse of the matrix. 
## This function assumes that the matrix is always invertible.
## First, the function checks if the inverse has already been computed. 
## If the inverse has been computed, cacheSolve() returns the inverse and skips the computation. 
## If the inverse has not been computed, the inverse is computed.
## The value is setted in the cache using setInverse() method.

cacheSolve <- function(m, ...) {
  
  ## Get the inverse of m
  inverse <- m$getInverse()
  
  ## Returns the inverse if it's already computed.
  if(!is.null(inverse)) {
    message("Getting cached data.")
    return(inverse)
  }
  ## Calculates and returns the inverse of the matrix if it has not been calculated yet
  else {
    ## Get the matrix
    matrix <- m$getMatrix()
    inverse <- solve(matrix)
    m$setInverse(inverse)
    
    return(inverse)
  }
  
}