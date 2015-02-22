## Overall, these functions create variables 
##"makeCacheMatrix" and "cacheSolve"
## To cache potentially time-consuming computations.
## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly


# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  # inv will store the cached inverse matrix
    inv <- NULL
  # Setter for the matrix 
    set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Getter for the matrix
    get <- function() x
  # Setter for the inverse
    setinverse <- function(inverse) inv <<- inverse
  # Getter for the inverse
    getinverse <- function() inv
  # Return the matrix with new defined functions
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve: Compute the inverse of the matrix. 
# The following function returns the inverse of the matrix. 
# It first checks if the inverse has already been computed. 
# If so, it gets the result and skips the computation.
# If not, it computes the inverse, sets the value in the cache via
# setinverse function.
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
  # If the inverse is already calculated, return it
    if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  # The inverse is not yet calculated, so we calculate it
  
    data <- x$get()
    inv <- solve(data)
  # Cache the inverse
  
    x$setinverse(inv)
  # Return it
    inv
}



## Sample run to test and show functionality:
x = rbind(c(1, -1/4), c(-1/4, 1))
m = makeCacheMatrix(x)
m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
