## To make a square matrix and compute the inverse of the square matrix  

## makeCacheMatrix() creates a matrix object mat that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL

    set <- function(y) {
        x <<- y
        mat <<- NULL
    }

    get <- function() x
    setMatrix <- function(solve) mat <<- solve
    getMatrix <- function() mat
    list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}

## cacheSolve() computes the inverse of the matrix object mat created by
## makeCacheMatrix() and retrieves the inverse from cache if the matrix has not 
## changed and is found. If not it will compute the inverse. 
cacheSolve <- function(x = matrix(), ...) { 
    mat <- x$getMatrix()

    if(!is.null(mat)) {
        message("getting cached data")
        return(mat)
    }

    data <- x$get()
    mat <- solve(data, ...)
    x$setMatrix(mat)
    mat
}
