## R programming assignment 2. Two functions, one to create a matrix object and one to invert it

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  set <- function(y){
    invs <<- NULL
    
    ## the cache is in invs - it's set to NULL first, and the <<- sets it for a different environment
    ## ..so it can be used elsewhere
    
    x <<- y
    invs <<- NULL
  
  }
  
  get <- function() x
  
    setInverse <- function(solveMatrix) 
    invs <<- solveMatrix
    getInverse <- function() invs
    
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    ## Call getInverse from above
    
    invs <- x$getInverse()

  if (!is.null(invs)) {
    
    message("getting cached data")
    
  return(invs)
}
  ## use an intermediate variable called intvar
  
  intvar <- x$get()
  invs <- solve(intvar,...)
  x$setInverse(invs)
  invs   
}

