## Put comments here that give an overall description of what your
## functions do

# these functions will allow a matrix to be created for which the 
# inverse can be defined and saved (cached) so it doesn't have to 
# be calculated over and over again when the result has already
# been solved and stored

## Write a short comment describing this function

# makeCacheMatrix is a function that takes a matrix as the argument
# and defaults to having the eventual inverse matrix defined as 
# 'NULL'. When instance$setinverse() is called, it will look
# to the global environment when defining variable 'm' within setinverse()
# and will use that value of 'm' when instance$getinverse() is called since it
# is set using instance$setinverse() by the global scoping convention "<<-".
# You *can* use instance$setinverse() directly, but the purpose of this exercise is
# to use instance$cacheSolve() to set that for us if it's not defined already.
# If you did define instance$setinverse directly, it would override 
# you trying to use instance$cachSolve() to set this because 'm' definition
# would not be 'NULL', and thus instance$cachSolve() would pull the cached 
# value without recalculating - see add'l notes in cacheSolve below

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
  
}

## Write a short comment describing this function

# cacheSolve() is used in conjuction with makeCacheMatrix() to
# define and save an inverse matrix - assuming no other arguements
# are passed when called - for a defined instance of makeCacheMatrix().
# If instance$getinverse() is not 'NULL' for that instance, then
# the function will use the solve() function on the matrix assigned in that instance
# of makeCacheMatrix() and further will return the definition of 'm' in the
# global environment from which instance$setinverse in makeCacheMatrix() will 
# pull from now so that the value can be saved when called on 
# instance$getinverse() on the go-forward for that instance- that is, unless it is
# redefined by a value that supercedes this, as in the example explained
# above in my makeCacheMatrix() notes.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
