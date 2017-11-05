## Script to handle cacheing a matrix inverse operation.

## Function to cache inverse of the matrix passed in

makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    set <- function(y) {
        x <<- y
        mat <<- NULL
    }

    get <- function() x
    setinverse <- function(solve) mat <<- solve
    getinverse <- function() mat

    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Solves inverse of matrix, returning cached value if exists

cacheSolve <- function(x, ...) {
        mat <- x$getinverse()
        if (!is.null(mat)) {
            message("getting cached value")
            return(mat)
        }

        data <- x$get()
        mat <- solve(data)
        x$setinverse(mat)
        mat
}
