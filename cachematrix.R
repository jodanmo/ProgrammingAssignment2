

##These functions creates the inverse of a matrix and cahes the results so they do not need to be recalcualted


## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix
library (MASS)
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(ginv) m <<- ginv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function calculates the inverse of the special "vector" created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- ginv(data, ...)
        x$setinv(m)
        m
}
