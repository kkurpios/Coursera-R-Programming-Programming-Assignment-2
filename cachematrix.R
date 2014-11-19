## Put comments here that give an overall description of what your functions do
## These two functions will create special object of the matrix class along with
## methods that will allow accessing this matrix and computing inversion,
## storing it in cache and retrieving the inversion from cache if the matrix
## hasn't changed.

## Write a short comment describing this function
## This function will allow users to create special matrix object and allow them
## to access it.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL     ## "inv" name stands for "inverted matrix"
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
}


## Write a short comment describing this function
## Following function will check whether the matrix inversion has been already
## computed and if not, it will do it by calling the solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Getting cached matrix inversion")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
