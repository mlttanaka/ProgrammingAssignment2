## The following functions calculate the inverse of a matrix.

## The makeCacheMatrix function takes a matrix, creates a special "matrix"
## object that can caches its inverse, and returns a list of the functions
## for the object: get(), set(), getinverse(), and setinverse().
makeCacheMatrix <- function(x = matrix()) {
    inverse_of_x <- NULL  # Initial value of the inverse of matrix 'x'.
    
    # Replace the elements of matrix "x" and reinitialize its inverse.
    set <- function(y) {
        x <<- y
        inverse_of_x <<- NULL
    }
    
    # Print elements of matrix "x" to the console.
    get <- function() x
    
    # Set the inverse of matrix "x".
    setinverse <- function(solve) inverse_of_x <<- solve
    
    # Print the inverse of matrix "x" to the console.
    getinverse <- function() inverse_of_x
    
    # Return a list of all the functions within makeCacheMatrix.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the special "matrix" 
## object returned by makeCacheMatrix. If the inverse has already been
## calculated and the matrix has not changed, the cacheSolve function
## retrieves the inverse from the special object's cache.
cacheSolve <- function(x, ...) {
    
    ## Get the inverse of object x's matrix from its cache.
    inverse_of_x <- x$getinverse()
    
    ## If the inverse of object x's matrix was retrieved from its cache,
    ## print a message to the console, and return the inverse of object x's
    ## matrix.
    if(!is.null(inverse_of_x)) {
        message("getting cached data")
        return(inverse_of_x)
    }
    
    ## Get the elements of object x's matrix.
    elements_of_x <- x$get()
    
    ## Calculate the inverse of object x's matrix.
    inverse_of_x <- solve(elements_of_x)
    
    ## Cache the inverse of object x's matrix in itself.
    x$setinverse(inverse_of_x)
    
    ## Return a matrix that is the inverse of object x's matrix.
    inverse_of_x
}

