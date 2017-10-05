## The objective is to cache the inverse of a matrix so that the 'matrix inverse' can be retrived  
## later (if necessary) from the cache instead of repeating the expensive computation. 

## The function makeCacheMatrix() creates a cache for the inverse (inv) of a matrix object 'x'

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
     x <<- y
     inv <<- NULL
   }
   get <- function() x
   setsolve <- function(solve) inv <<- solve
   getsolve <- function() inv
   list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}

## The function cacheSolve() computes the inverse (inv) of the matrix returned by makeCacheMatrix() 

cacheSolve <- function(x, ...) {
        
  inv <- x$getsolve()  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}
