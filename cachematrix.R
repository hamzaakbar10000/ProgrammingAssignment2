## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                       ## initialized inv as NULL. It will store the value of matrix inverse 
  set <- function(y) {              ## defined the set function to assign new 
    x <<- y                         ## value of matrix in the parent environment
    inv <<- NULL                    ## inv will reset to NULL if there is a new matrix 
  }
  get <- function() x               ## defined the get fucntion which returns value of the matrix argument
  
  setinverse <- function(inverse) inv <<- inverse  ## It will assign value of inv in parent environment
  getinverse <- function() inv                     ## It will get the value of inv where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## Write a short comment describing this function
## This function computes the inverse of special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
