## This functions creates a special matrix and its cached inverse


## This function creates a special Matrix object that can cache its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  get <- function() x
  
  setInverseMatrix <- function(inverse) {
    inverseMatrix <<- inverse
  }
  
  getInverseMatrix <- function() inverseMatrix
  
  list(set = set, get = get, setInverseMatrix = setInverseMatrix, 
       getInverseMatrix = getInverseMatrix)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getInverseMatrix()
  
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data,...)
  x$setInverseMatrix(i)
  i
}
