## This function creates a special matrix object that can cache it's inverse.
## It's functions are :
## a) set the value of the matrix
## b) get the value of the matrix
## c) set the value of the inverse
## d) get the value of the inverse
## e) returns a list containing the above four functions (set, get, setInverse, 
##    getInverse respectively)

makeCacheMatrix <- function(x = matrix()) {
      a <- NULL
      set <- function(y) {
            x <<- y
            a <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) a <<- inverse
      getInverse <- function() a
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## =============================================================================
## This function calculates the inverse of the special matrix created by the
## makeCacheMatrix() function, after checking if the inverse has already been
## calculated. If so, then it gets the inverse from the cache and skips the
## computation. Else, it calculates the inverse of the matrix and sets it to the 
## setInverse() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      a <- x$getInverse()
      if (!is.null(a)) {
            message("getting cached data ....")
            return a
      }
      mat <- x$get()
      a <- solve(mat, ....)
      x$setInverse(a)
      a
}
