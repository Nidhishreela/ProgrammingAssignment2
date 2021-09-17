## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix function is used to store the values for any function.
# so that it can be retrived when same values are called in function

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinver <- function(inv) inver <<- inv
  getinver <- function() inver
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
  
}


## Write a short comment describing this function
# this function checks if the value is computed already, if computed gets the value from cache
# f not computes and stores the value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getinver()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setinver(inver)
  inver
}
