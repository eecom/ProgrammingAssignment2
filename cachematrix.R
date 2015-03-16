makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(yy) m <<- yy
    getInverse <- function() m
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
	m <- x$getInverse()
    if(!is.null(m)) {
	    message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}

## b = makeCacheMatrix(matrix(c(2, 4, 3, 1, 5, 7, 9, 6, 1), nrow=3, ncol=3))
## cacheSolve(b)
