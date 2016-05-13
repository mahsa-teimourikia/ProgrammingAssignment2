## This is Assignment 2 of the course R programming
## The goal is to Cache the inverse of a matrix

#' This function creates a special "matrix" object that can cache its inverse.
#' 
#' @param x A matrix.
#' @return A list.
#' @examples
#' x  <- diag(4,4)
#' makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(inverse) inv <<-inverse
        
        getInverse <- function() m
        
        list(set = set, 
             get = get, 
             setInverse= setInverse, 
             getInverse = getInverse)
}


#' This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
#' above. If the inverse has already been calculated (and the matrix has not changed), then 
#' the cachesolve should retrieve the inverse from the cache.
#' 
#' @param x A cacche matrix returned by makeCacheMatrix
#' @param ... parameters to pass to solve
#' @return inverse of a matrix
#' @example 
#' CachedMarix <- makeCacheMatrix(x)
#' cacheSolve(CachedMarix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInverse()
        
        if(!is.null(inv)){
                message("getting the matrix inverse from the cache...")
                return(inv)
        }
        
        data <- x$get()
        
        inv <- solve(data, ...)
        
        x$setInverse(inv)
        
        inv
}


