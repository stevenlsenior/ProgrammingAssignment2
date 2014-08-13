## The code below creates two functions for cacheing the inverse of a matrix

## This function creates an R object, makeCacheMatrix, which contains functions to: 
## set the matrix; get the matrix; set the inverse of the matrix; and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
  x <<- y
  i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}


## This function: (1) checks whether the inverse of the matrix has already been calculated;
## and (2) if it hasn't, it calculates the inverse and sets it with setinverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data  <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
