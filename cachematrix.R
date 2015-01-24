## The following two functions together facilitate the computation of the inverse of an invertible matrix 
## by allowing the system to check whether the inverse matrix has been previously cached and return that result. 
## Or, if not cached, to compute the inverse matrix and cache that result for future computations of the same inputted matrix. 
## For this exercise, it is assumed that the matrix supplied is always invertible.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse. 
## This function also returns functions passed to the cacheSolve function below to get the existing cached value of the inverse
## matrix or set the newly calculated value. 

makeCacheMatrix <- function(x = matrix()) {   
        im <- NULL                  
        set <- function(y) {        
                x <<- y             
                im <<- NULL          
        }
        get <- function() x
        setsolve <- function(solve) im <<- solve
        getsolve <- function() im
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}

## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix() above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve() retrieves the inverse 
## from the cache.
## If not cached, the function computes the inverse of the matrix and sets the value in the cache via the setsolve function.
## In both cases, the cacheSolve function returns a matrix that is the inverse of the matrix inputted.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
        im <- x$getsolve()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)   
                  
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setsolve(im)
        im
        
}
