## Put comments here that give an overall description of what your
## makeCacheMatrix set up an environnment with the data and functions to access it from the matrix parameter 

## set up the environment with the data and functions for matrix argument
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function( y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setInverse <- function( solve) m <<- solve
        getInverse <- function() m
        list( set = set, get = get,
              setInverse = setInverse,
              getInverse= getInverse)
}


## cacheSolve calculate and store the inverse matrix in the environment created with makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if( ! is.null( m)) {
                message("getting cached data")
                return( m)
        }
        data <- x$get()
        m <- solve( data, ...)
        x$setInverse( m)
        m
}

