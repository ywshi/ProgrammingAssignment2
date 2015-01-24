## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmatrix <- function(x2) { x <<- x2;inv <<- NULL }
  getmatrix <- function() { x }
  setinverse <- function(inverse) { inv<<-inverse  }
  getinverse <- function() {inv}
  
  #list of functions can be called from makeCacheMatrix$
  list(setm=setmatrix, getm=getmatrix, seti=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("return cached data.")
    return(inv)
  }
  data <- x$getm()
  inv <- solve(data)
  x$seti(inv)
  inv
}
