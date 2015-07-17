## Matrix inversion is a costly computational operation.
## Hence, we implement a caching solution where the inverse 
## of a matrix is cached (temporarily stored in the working environment)
## so that its automatically recalled from memory whenver we attempt
## to compute the inverse of a particular more than once.
## This implementation takes advantage of the lexical scoping of the R langugage.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    matrix_inv <- NULL
    set <- function(y) {
        x <<- y
        matrix_inv <<- NULL
    }
    get <- function() x
    setmatrixinv <- function(m_inv) matrix_inv <<- m_inv
    getmatrixinv <- function() matrix_inv
    list(set = set, get = get,
         setmatrixinv = setmatrixinv,
         getmatrixinv = getmatrixinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve function retrieves the 
## inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matrix_inv <- x$getmatrixinv()
    if(!is.null(matrix_inv)) {
        message("getting cached data")
        return(matrix_inv)
    }
    data_matrix <- x$get()
    matrix_inv <- solve(data_matrix, ...)
    x$setmatrixinv(matrix_inv)
    matrix_inv
}
