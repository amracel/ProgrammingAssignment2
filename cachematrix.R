## cachematrix.R
## creates a special matrix 'class' then provides a method
## for returning the inverse of the matrix argument.
## inverse is cached. if operations are for larger matrices,
## offers a way to return stored value, rather than recalculating

## Create matrix object that includes setter and getter
## methods for both matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Returns reverse of matrix 'object'. Caches values
## so, if called again on same value, can return it
## without recalculating.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m 
}
