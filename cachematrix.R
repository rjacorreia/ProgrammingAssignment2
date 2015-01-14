## Put comments here that give an overall description of what your
## functions do

#Below are two functions that are used to create a special object that stores a matrix and cache's its inverse matrix.

## Write a short comment describing this function
#The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#The following function calculates the mean of the special "vector" created with the above function. 
#However, it first checks to see if the inverse matrix has already been calculated. 
#If so, it gets the cached inversed matrix and skips the computation. 
#Otherwise, it calculates the new inversed matrix and sets the current inversed matrix in the cache via the setinverse function.


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
