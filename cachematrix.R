## This function calculates the inverse of a matrix
## and stores it as a special object.


makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set,
       get=get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## If the function before has already calculated the
## inverse of the matrix it returns it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
  
