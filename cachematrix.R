## makeCacheMatrix and cacheSolve work together to cache
## the inverse of a matrix to save the work of 
## recomputing it.

## function: makeCacheMatrix
## Creates an object that stores and returns the 
## inverse of a matrix.
##
## Example:
## > my_cache = makeCacheMatrix()
## > y = matrix(c(1,2,-2,1),2,2)
## > my_cache$set(y)
## > my_cache$get()
##      [,1] [,2]
## [1,]    1   -2
## [2,]    2    1
## > my_cache$setinverse(solve(y))
## > my_cache$getinverse()
##      [,1] [,2]
## [1,]  0.2  0.4
## [2,] -0.4  0.2

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## function: cacheSolve
## input: matrix and cache object created by makeCacheMatrix
## If inverse of matrix already exists in cache, return
## that inverse. Otherwise, compute the inverse, store it
## in the cache, and return it.
##
## Example:
## > my_cache = makeCacheMatrix()
## > y = matrix(c(1,2,-2,1),2,2)
## > my_cache$setinverse(NULL)
## > cacheSolve(y, my_cache)
##      [,1] [,2]
## [1,]  0.2  0.4
## [2,] -0.4  0.2
## > cacheSolve(y, my_cache)
## getting cached data
##      [,1] [,2]
## [1,]  0.2  0.4
## [2,] -0.4  0.2

cacheSolve <- function(x, cache) {
        ## Return a matrix that is the inverse of 'x'
		inv <- cache$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- cache$get()
        inv <- solve(mat)
        cache$setinverse(inv)
        inv
}
