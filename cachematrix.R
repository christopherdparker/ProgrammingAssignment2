## These functions calculate the inverse of a matrix. If the inverse has already been calculated
## the functions return the previously calculated inverse, saving computational time.

## This function enables saving of a matrix inverse if it has already been calculated.

makeCacheMatrix <- function(x = matrix()) {
    # Create a matrix that includes information the inverse of the initial matrix
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse of a matrix.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
