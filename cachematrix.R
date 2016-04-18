## The below two functions are used to check if the inverse of a matrix been created or not
## and if it isn't created the function will create one (assuming the given matris is invertiable)

## This function has the necessary parameter that will help us decide whether the inverse is calculated or not.

makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function will help us to calculate the inverse of the matrix using solve(x) function

cacheSolve <- function(x, ...) {
       inverse <- x$getInverse()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        mat <- x$get()
        inverse <- solve(mat, ...)
        x$setInverse(inverse)
        inverse
}
