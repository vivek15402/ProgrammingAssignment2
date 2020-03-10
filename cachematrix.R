## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.
## Initialize inverse with NULL then initialize x to get_matrix function, initialize setInverse and so on...

makeCacheMatrix <- function(x = matrix()) {             ## define the argument with default mode of matrix
        inverse <- NULL
        set_matrix <- function(y){                      ## define the set function
                x <<- y                                 ## This represents value of matrix in parent environment
                inverse <<- NULL                        ## to reset inverse to null if there is a new matrix
        }
        get_matrix <- function() x                      ## returns the value of matrix argument
        setInverse <- function(solveMatrix) inverse <<- solveMatrix     ## assign matrix value to parent environment
        getInverse <- function() inverse                                ## it gets the value of inverse where called
        list(set_matrix = set_matrix, get_matrix = get_matrix, setInverse = setInverse, getInverse = getInverse)  ## used to refer tp the function woth $ operator
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## if the inverse oid already calculated then inverse matrix will be retreived from cache
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
