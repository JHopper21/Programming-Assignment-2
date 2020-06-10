## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##creates a special matrix object that can cahce its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y)
    ##set the value of the matrix
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  ##get the value of the matrix
  setinv <- function(inverse) inv <<- inverse
  #set the value of inverse
  getinv <- function() inv
  ##get the value of the inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function
##computes the inverse of the special "matrix" returned by makeCacheMatrix above
##If the inverse has already been calculated(and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv))
    {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
