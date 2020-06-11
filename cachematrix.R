## A pair of functions that compute and cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## First, let's initialize the variables x (a matrix)
  ## and inverse (it's inverse)
  inverse <- NULL
  ## Let's create a function that can get the value of the matrix.
  getM <- function() x
  ## Create a function that can cache the inverse of a matrix.
  setInverse <- function(inv) inverse <<- inv ## Again, we use the 
  ## operator for caching the new value of 'inverse' in the parental
  ## environment.
  ## Our last function is going to get the inverse of the matrix
  getInverse <- function() inverse
  ##Finaly, our function return a list of the previous functions 
  ## so later we can access any object defined in the environment of 
  ## 'makeCacheMatrix'
  list(getM = getM, setInverse = setInverse, getInverse = getInverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse() ## initializing a variable with the currrent value 
  ## of the inverse of 'x'
  
  if(!is.null(inverse)) { ## Checks if the inverse has already been calculated
    message("getting cached data") ## if so, return the value of inverse
    return(inverse)
  }
  ## Else, we use the matrix 'x' to calculate and return its inverse
  myMatrix <- x$getM()
  inverse <- solve(myMatrix, ...) ## we use the '<-0 operator because we are updating
  ## a variable that already exists in this environment
  x$setInverse(inverse)
  inverse
}
