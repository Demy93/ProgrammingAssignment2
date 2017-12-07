## These functions together can calculate and cache the inverse of a matrix


## The following function creates a "matrix" object, or a list of functions, 
## that can calculate and cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) i <<- solve
     getinverse <- function() i
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## The following function is the part that retrieves or calculates the 
## inverse of the matrix object defined in the function above. It retrieves 
## the inverse from the cache if the inverse has been calculated before, 
## and otherwise it calculates and caches the inverse.

cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data,...)
     x$setinverse(i)
     i   ## Return a matrix that is the inverse of 'x'
}