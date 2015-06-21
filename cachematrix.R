## makeCacheMatrix() function takes a matrix x as an argument, and returns a list of functions to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix
## Please note that the argument matrix 'x' is assumed to be a square matrix as per the assignment spec
## Example Usage:
## makeCacheMatrix(matrix(c(4, 2, 7, 6), nrow=2, ncol=2))
##
## cacheSolve() function calculates the mean of the special "matrix" created with the makeCacheMatrix() function.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it computes the inverse and sets the value of the inverse in the cache via the setinverse function.

## Function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # Holds the inverse matrix, initially set to null
    inv <- NULL
    
    # Internal function to let the user cache the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Internal function to let the user get the cached matrix
    get <- function() x
    
    # Internal function to let the user cache the inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse
    
    # Internal function to let the user get the cached inverse of the matrix
    getinverse <- function() inv
    
    # Return a list with the internal operations
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function to compute the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
    # Get the inverse
    inv <- x$getinverse()
    
    # If it's cached, return the cached response
    if(!is.null(inv)) {
        message("Returning a cached response:")
        return(inv)
    }
    
    # If it's not cached, get the data, compute its inverse, and cache it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    
    # Return the response, a matrix that is the inverse of 'x'
    inv
}
