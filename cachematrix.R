## Put comments here that give an overall description of what your
## functions do

## This function creates an 'object' which will hold both the matrix it is
## passed as an argument and its inverse, together with four 'methods' to get
## and set the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function calls the getinverse 'method' of a makeCacheMatrix 'object'. If
## a matrix is returned it outputs that, otherwise it calls the get 'method' to 
## get the original matrix and creates an inverse from that, which it then
## outputs and stores in the makeCacheMatrix 'object' by calling the setinverse
## 'method'.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
