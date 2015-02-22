## The goal of this assignment was to create an R function that is able to cache potentially time-consuming computations:
## in this example, taking the inverse of a matrix. 
## If the contents of a matrix is not changing, it may make sense to cache the value of the ivert so that when 
## we need it again, it can be looked up in the cache rather than recomputed. 

## makeCacheMatrix creates a matrix and functions to
## set the matrix
## get the matrix
## set inverse of the matrix
## get inverse of the matrix


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


## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache

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

