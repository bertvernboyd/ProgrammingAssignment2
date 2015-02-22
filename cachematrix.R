## Creates a matrix which can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {  
    x <<- y
    inv <<- NULL  ## Removes cached inverse if new matrix is set        
  }
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  
  list(set = set, get = get, 
       set_inverse = set_inverse,
       get_inverse = get_inverse) 
}


## Returns the inverse of a matrix created with makeCacheMatrix()
cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if(!is.null(inv)) {                  ## If the inverse has been already calculated return the cached result
    message("getting cached data")  
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)  ## Calculcate inverse otherwise
  x$set_inverse(inv)
  inv
}
