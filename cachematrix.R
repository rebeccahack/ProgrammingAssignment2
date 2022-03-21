## two functions for caching a matrix and its inverse

## makeCacheMatrix
## takes a matrix as its input
## returns a list of four functions:
## set - overwrite the matrix with a new matrix
## get - return the stored matrix
## set_inverse - calculate and store the inverse of the stored matrix
## get_inverse - return the calculated inverse (returns null if no inverse has been calculated for the most recently inputted matrix)

makeCacheMatrix <- function(x = matrix()) {
    x_inverse <- NULL
    set <- function(y) {
        x <<- y
        x_inverse <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) x_inverse <<- solve(x)
    get_inverse <- function() x_inverse
    invisible(list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse))
}


## cacheSolve
## takes a CacheMatrix (generated using makeCacheMatrix) as an input (x)
## returns the inverse of the matrix stored in x

cacheSolve <- function(x) {
        inverse = x$get_inverse()
        if (is.null(inverse)) {
            inverse <- solve(x$get())
            x$set_inverse(inverse)
        }
        invisible(inverse)
}
