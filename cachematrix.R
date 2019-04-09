## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function will create a list of functions that will help us to store and manipulate the matrix on the
# global environment to load the matrix when it has been calculated previously
makeCacheMatrix <- function(x = matrix()) {
  mtrx <- NULL
  set <- function(y) {
    x <<- y
    mtrx <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) mtrx <<- matrix
  getmatrix <- function() mtrx
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Write a short comment describing this function

# This function will view if the inverse of the matrix was already calculated and if its not it will calculates it and # # then store it in the global variables and return it
cacheSolve <- function(x, ...) {
  mtrx <- x$getmatrix()
  if(!is.null(mtrx)) {
    message("getting cached data")
    return(mtrx)
  }
  data <- x$get()
  mtrx <- solve(data, ...)
  x$setmatrix(mtrx)
  ## Return a matrix that is the inverse of 'x'
  mtrx
}