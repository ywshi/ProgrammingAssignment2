#return a list of functions
makeCacheMatrix <- function(m1 = matrix()) {
  inv <- NULL
  setmatrix <- function(m2) { m1 <<- m2;inv <<- NULL }
  getmatrix <- function() { m1 }
  setinverse <- function(inverse) { inv<<-inverse  }
  getinverse <- function() {inv}
  
  #list of functions can be called from makeCacheMatrix$
  list(setm=setmatrix, getm=getmatrix, seti=setinverse, geti=getinverse)
}

# perform inverse of the matrix which is assumed invertible.
cacheSolve <- function(mcm, ...) {
  inv <- mcm$geti()
  if(!is.null(inv)) {
    message("return cached data.")
    return(inv)
  }
  data <- mcm$getm()
  inv <- solve(data)
  mcm$seti(inv)
  inv
}