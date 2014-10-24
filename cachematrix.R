## Put comments here that give an overall description of what your
## functions do

## Creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Method to set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Method to get the matrix
  get <- function() x
  
  ## Method to set the inverse of the matrix
  setmatrix <- function(inverse) m <<- inverse
  
  ## Method to get the inverse of the matrix
  getmatrix <- function() m
  
  ## Return a list of the methods
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Computes the inverse of the matrix returned by "makeCacheMatrix". If the inverse has already been calculated
##then it will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  ## Return the inverse if its already set
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the data
  data <- x$get()
  
  ## Use the solve function to return the inverse of the data
  m <- solve(data, ...)
  
  ## Set the inverse
  x$setmatrix(m)
  
  ## Return the inverse matrix
  m
  
}
