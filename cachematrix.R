## This is programing assginment 2 for prog-011 R class
## The purpose of this assignment is work on the lexical scooping aspects of R
## This function is designed to create a matrix object outside the environment that
## calculates the inverse of the matrix

## The 'makeCacheMatrix is designed to make a matrix and create a cache version of the inverse of the matrix for
## use by the second part of this assignment.  
makeCacheMatrix <- function(x = matrix()) {
        z <- NULL
        set <- function(y) {
                x <<- y
                z <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) z <<- solve
        getinverse <- function() z
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## 'cacheSolve' will return a inverse matrix of x
cacheSolve <- function(x, ...) {
        z <- x$getinverse()
        if(!is.null(z)) {
                message("using cached data")
                return(z)
        }
        data <- x$get()
        z <- solve(data, ...)
        x$setinverse(z)
        z
}
