## Assignment 2: Caching the Inverse of a Matrix
##
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Write a pair of functions that cache the inverse of a matrix. 


makeCacheMatrix <- function(x = matrix()) {
        # Initially set to NULL
        output <- NULL

        #Set the matrix
        set <- function(y) {
                x <<- y
                output <<- NULL
        }

        # Get the matrix
        get <- function() x

        # Set the inverse of the matrix
        setInverse <- function(inverse) output <<- inverse

        # Get the inverse of the matrix
        getInverse <- function() output

        # Wrap up into a list
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}

# Find a matrix inverse 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        output <- x$getInverse()

        if(!is.null(output)) {
                message("Getting cached matrix")
        }
        else {
                data <- x$get()

                output <- solve(data)

                x$setInverse(output)
        }
        return(output)
}

#*** Examples  ***#

#  >   y <- matrix(rnorm(16), 4)
#  >   n = makeCacheMatrix(y)
#  >   n$get()
#             [  ,1]     [  ,2]      [  ,3]      [  ,4]
#  [  1,] -1.6145584  1.4093630  1.10091783 -0.02735241
#  [  2,] -0.3650492  1.0935646 -0.04644600 -0.79273014
#  [  3,]  1.0773857  0.4567007 -0.03044847 -0.73238182
#  [  4,]  1.1486415 -0.6715796 -1.09638877 -0.95156542
#  >   n$getInverse()
#  NULL
#  >   cacheSolve(n)
#             [  ,1]      [  ,2]      [  ,3]     [  ,4]
#  [  1,] -0.5763911  0.06857468  0.72937796 -0.6019331
#  [  2,] -1.4312026  1.79529435  0.07910059 -1.5153650
#  [  3,]  1.8500557 -2.16515773  0.96137860  1.0106358
#  [  4,] -1.8173008  1.31040838 -0.28308431 -1.8724586
#  >   cacheSolve(n)
#  Getting cached matrix
#             [  ,1]      [  ,2]      [  ,3]     [  ,4]
#  [  1,] -0.5763911  0.06857468  0.72937796 -0.6019331
#  [  2,] -1.4312026  1.79529435  0.07910059 -1.5153650
#  [  3,]  1.8500557 -2.16515773  0.96137860  1.0106358
#  [  4,] -1.8173008  1.31040838 -0.28308431 -1.8724586
#  >   z <- cbind(y, c(1,2,3,4))
#  >   n$setInverse(z)
#  >   n$get()
#             [  ,1]     [  ,2]      [  ,3]      [  ,4]
#  [  1,] -1.6145584  1.4093630  1.10091783 -0.02735241
#  [  2,] -0.3650492  1.0935646 -0.04644600 -0.79273014
#  [  3,]  1.0773857  0.4567007 -0.03044847 -0.73238182
#  [  4,]  1.1486415 -0.6715796 -1.09638877 -0.95156542
#  >   n$getInverse()
#             [  ,1]     [  ,2]      [  ,3]      [  ,4] [  ,5]
#  [  1,] -1.6145584  1.4093630  1.10091783 -0.02735241    1
#  [  2,] -0.3650492  1.0935646 -0.04644600 -0.79273014    2
#  [  3,]  1.0773857  0.4567007 -0.03044847 -0.73238182    3
#  [  4,]  1.1486415 -0.6715796 -1.09638877 -0.95156542    4
#  >   cacheSolve(n)
#  Getting cached matrix
#             [  ,1]     [  ,2]      [  ,3]      [  ,4] [  ,5]
#  [  1,] -1.6145584  1.4093630  1.10091783 -0.02735241    1
#  [  2,] -0.3650492  1.0935646 -0.04644600 -0.79273014    2
#  [  3,]  1.0773857  0.4567007 -0.03044847 -0.73238182    3
#  [  4,]  1.1486415 -0.6715796 -1.09638877 -0.95156542    4
