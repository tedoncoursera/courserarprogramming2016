

## This function creates a cache in memory for the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
         x <<- y
         m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

}



## this code computes inverse of matrix 'x' if x inverse not already stored in memory

cacheSolve <- function(x, ...) {
       ## Return a inverse matrix of 'x'
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

