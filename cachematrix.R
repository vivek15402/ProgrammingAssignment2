## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.
## Initialize inverse with NULL then initialize x to get_matrix function, initialize setInverse and so on...

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set_matrix <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get_matrix <- function() x
        setInverse <- function(solveMatrix) inverse <<- solveMatrix
        getInverse <- function() inverse
        list(set_matrix = set_matrix, get_matrix = get_matrix, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <- x$get_matrix()
        inverse <- solve(data)
        x$setInverse(inverse)
        inverse      
}

## Output of the code

## m <- matrix(c(1,2,3,4),2,2)
## m1 <- makeCacheMatrix(m)
## cacheSolve(m1)
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
