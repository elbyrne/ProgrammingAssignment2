## The functions in this file create an object that consists of a matrix, its
## inverse, and a series of functions to get and set their values

## This function takes the matrix x and adds to it four functions: get and set 
## (which store and return the matrix) and getinv and setinv (which store and
## return the matrix inverse)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
} 

## Because the inverse of a large matrix is expensive to compute we want to
## cache its value. This function first checks the object created in
## makeCacheMatrix() to see whether the inverse has been calculated. If it has been
## calculated previously, the cached value is returned. If not, it
## calculates the inverse and stores it in x using the setinv method

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
