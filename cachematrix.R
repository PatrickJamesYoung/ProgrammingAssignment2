## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix function will turn a matrix into a special 
## "matrix" that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        ## Creates a special 'matrix' whose cache can be stored
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function
## The cacheSolve function will compute the inverse of the matrix created
## above and, if the inverse has already been calculated, it will
## retrieve the inverse from the cache. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
