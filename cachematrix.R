# The functions calculate the inverse of a matrix and cache it
# After obtaining the inverse for the first time, if the function is called again 
# the inverse is retrieved from memory 

cachesolve <- function(x, ...){
  # it returns the inverse of a matrix
  # first it checks whether it was calculated, if yes the inverse is loaded from memory
  # otherwise the inverse is calculated
  invM <- x$getinv()
  if(!is.null(invM)){
    message("getting cached data")
    return(invM)
  }
  data <- x$get()
  invM <- solve(data, ...)
  x$setinv(invM)
  invM
}

makecacheMatrix <- function(x = matrix()) {
  # it gets a matrix and keep a version of it along with its inverse
  m1 <- NULL
  set <- function(y) {
    x <<- y
    m1 <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m1 <<- inv
  getinv <- function() m1
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

