# Matrix inversion can be a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than computing it repeatedly 
# the functions hear facilitate cacheing the inverse of a matrix.

# function makeCacheMatrix - create matrix object that can cache its inverse - returns list
# makeVector creates a special "vector", which is really a list containing functions to:

# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setinvrs <- function(inverse) invrs <<- inverse
        getinvrs <- function() invrs
        list(set = set, get = get,
             setinvrs = setinvrs,
             getinvrs = getinvrs)
}

# function cacheSolve - computes inverse of object returned by makeCacheMatrix
# if already calculated then return from cache and print message to console
# matrix must be a square invertible matrix
# use solve() to return inverse

cacheSolve <- function(x, ...) {
        invrs <- x$getinvrs()
        if(!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
        data <- x$get()
        invrs <- solve(data, ...)
        x$setinvrs(invrs)
        invrs
}

# test functions / output from functions below:

> source('C:/R/code/makeCacheMatrix.R', echo=TRUE)

> billmat=matrix(c(2,4,6,8),2,2)
> billmat
     [,1] [,2]
[1,]    2    6
[2,]    4    8

> x=matrix(c(2,4,6,8),2,2)
> x
     [,1] [,2]
[1,]    2    6
[2,]    4    8
> billmat <- makeCacheMatrix(x)

> billmat$get()
     [,1] [,2]
[1,]    2    6
[2,]    4    8

> billmat$getinvrs()
NULL

> source('C:/R/code/cacheSolve.R', echo=TRUE)

# not cached 
> cacheSolve(billmat)
     [,1]  [,2]
[1,] -1.0  0.75
[2,]  0.5 -0.25

# cached 
> cacheSolve(billmat)
getting cached data
     [,1]  [,2]
[1,] -1.0  0.75
[2,]  0.5 -0.25

# get inverse 
> billmat$getinvrs()
     [,1]  [,2]
[1,] -1.0  0.75
[2,]  0.5 -0.25

# change matrix values ;
> x=matrix(c(4,3,2,1),2,2)
> billmat$set(x)
> billmat$get()
     [,1] [,2]
[1,]    4    2
[2,]    3    1
> billmat$getinvrs()
NULL

# not cached
> cacheSolve(billmat)
     [,1] [,2]
[1,] -0.5    1
[2,]  1.5   -2

# cached
> cacheSolve(billmat)
getting cached data
     [,1] [,2]
[1,] -0.5    1
[2,]  1.5   -2

# new inverse
> billmat$getinvrs()
     [,1] [,2]
[1,] -0.5    1
[2,]  1.5   -2
