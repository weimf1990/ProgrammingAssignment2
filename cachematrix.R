## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function: I created the matrix; the variables set, get, setinverse, getinverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function: I continued by creating the cacheSolve function. This will endup return the inverse matrix we want. 

cacheSolve <- function(x, ...) {
        
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  ## Return a matrix that is the inverse of 'x'
  m
}

##test
test_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
test_matrix$get()
test_matrix$getinverse()
cacheSolve(test_matrix)
