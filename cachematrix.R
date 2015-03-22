## Put comments here that give an overall description of what your
## functions do

## This function overall caches the inverse of a matrix. The function 
## makeCacheMatrix crates a matrix object that caches its inverse.
## The second function cacheSolve computes the inverse of the matrix
## returend by the first function, using the function solve in R.

## Write a short comment describing this function

## makeCacheMatrix uses the operator << to assign values in a different
## environment. The function simply creates a new special matrix object 
## that caches its own inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(setmatrix = setmatrix, 
             getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve calculates the inverse of the matrix returend from the 
## function above. IF the inverse was already calculated, this function
## only retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
