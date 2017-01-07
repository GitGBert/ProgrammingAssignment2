## Cache of Matrix Inversion 
## Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than
## compute it repeatedly. This assignment is to write a pair of
## functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        
        set <- function(y) {
                
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinvert <- function(solve) m <<- solve
        
        getinvert <- function() m
        
        list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)   

}


 
## The following function calculates the inverse of the special "matrix" created with
## the above function. However, it first checks to see if the inverse matrix has already been
## calculated. If so, it gets the inverse matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse matrix of the data and sets the value of the inverse 
## matrix in the cache via the setinvert function.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
        m <- x$getinvert()
        
        if(!is.null(m)) {
                
                message("getting cached data")
                
                return(m)
        }
        
        data <- x$get()
        
        m <- solve(data, ...)
        
        x$setinvert(m)
        
        m  
} ## the cacheSolve function is defined within the scoping of the makeCacheMatrix function
