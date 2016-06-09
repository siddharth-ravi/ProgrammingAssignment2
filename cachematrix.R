## The following functions are being addressed in this program

#makeCacheMatrix: This function creates a special "matrix" object that can cache 
#its inverse.

#cacheSolve: This function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then cacheSolve should retrieve the inverse 
#from the cache.

# 1. Function makeCacheMatrix creates a function that can cache inverse values

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x  
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# 2. Function cacheSolve returns the inverse of a matrix - from cache if available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
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
