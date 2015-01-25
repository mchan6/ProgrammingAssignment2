## Calculating Matrix inverses can be a time-consuming computation. Thus, 
## one can be more resourceful by caching the inverse of a matrix.
## The two functions below are used to create and cache the inverse of a matrix.


## makeCacheMatrix This function creates a special "matrix" object that can cache its inverse.
## It creates a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function below computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has alreadybeen calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## This function assumes that the matrix is always invertible.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
