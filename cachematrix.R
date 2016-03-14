## The functions defined below allow the user to find the inverse of the matrix.
## Inverse is cached so that if the matrix inverse has already been calculated,
## the function will pull the value from cache instead of re-computing


## This function creates a custom "Matrix" object, which is a list containing
## functions to set the value of a matrix, get the value of a matrix, set the
## value of the inverse of the matrix, and get the value of the inverse of the
## matrix. The input is a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(solve) m <<- solve
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function computes the inverse of a matrix using solve(). However, if
## the inverse of the matrix has been previously computed and stored in cache,
## it will pull the value from cache instead of recomputing. The input is the
## special "Matrix" object output by the makeCacheMatrix() function.

cacheSolve <- function(x, ...) {
        ## Check if the matrix inverse has already been computed and pull from
        ## cache if it has
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## If the matrix inverse is not stored in cache
        ## return a matrix that is the inverse of the matrix store in "matrix"
        ## object 'x'
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}
