## This file contains two functions that can be used to invert a matrix
## It is assumed that the matrix that is used is square and invertable

## This function writes a special matrix that really is a list containing functions to
## Set the value of the matrix
## Get the value of the matrix
## Set the value of the inverse matrix
## Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
   
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

    
}


## This function gets the inverse matrix of the "special" matrix from above (MakeCacheMatrix)
## If the inverse of this matrix has already been calculated before it returns the previously
## calculated inverse with the text "getting cached data"
## Otherwise the inverse is calculated using the "solve" function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
    
}
