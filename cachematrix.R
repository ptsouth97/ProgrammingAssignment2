## This function creates a special "matrix" object that can cache its inverse.
## Can use this test matrix:  matrix(c(-1, -2, 1, 1), 2, 2)

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL                                         # will be the cached value
  
  set <- function(y) {                              # set the value of the matrix
    x <<- y                                         # <<- means super assignment
    m <<- NULL
  } 
  
  get <- function() { x }                           # returns the value of the original matrix
  
  setinverse <- function(inverse) { m  <<- inverse }      
                                                     # sets the value of the inverse..called by
                                                     # cacheSolve() access and stores the 
                                                     # value during superassignment
  
  getinverse <- function() { m }                     # returns the cached value to cacheSolve
                                                     # on subsequent accesses
  
  list(set = set, get = get,                         # accessed each time makeCacheMatrix is called.
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {                     # Return a matrix that is the inverse of 'x'
        
  
  m <- getinverse()                                  # access the object 'x' and get the inverse
        
  if(!is.null(m)) {                                  # if inverse was already cached (not NULL)...
    message("getting cached data")                   # ...send this message to the console
    return(m)                                        # ... and return the inverse..."return" ends
  }                                                  # the function cacheSolve()
    
  data <- get()                                      # we reach this code only if getinverse() returned NULL
  m <- solve(data, ...)                              # if m was NULL then calculate the inverse
  setinverse(m)                                      # store the inverse in x (setinverse())
  m                                                  # return the inverse to the code that called this function
}
  
