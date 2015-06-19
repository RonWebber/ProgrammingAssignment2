## The functions 'makeCacheMatrix' and 'cacheSolve' together are a cache for a 
## matrix and its inverse. The matrix and its inverse are cached in the function
## 'makeCacheMatrix'. The function 'cacheSolve' computes the inverse, sends it
## to 'makeCacheMatrix', and accesses and returns the inverse.

## The function 'makeCacheMatrix' takes a matrix 'x' and returns a list of four
## functions, 'l', that 'cacheSolve' uses to return the inverse of the matrix 'x'.

makeCacheMatrix <- function(x = matrix(1)) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The function 'cacheSolve' inputs 'l', the output of 'makeCacheMatrix', and 
## returns the inverse of the matrix 'x'. If the inverse is in the environment 
## of the body of 'makeCacheMatrix', then 'cacheSolve' retrieves it; if not, 
## 'cacheSolve' computes the inverse, caches it in 'makeCacheMatrix', and then
## returns it.

cacheSolve <- function(l) {
        ## Return a matrix that is the inverse of the matrix 'x'.
        i <- l$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        } else {
                data <-l$get()
                inverse <- solve(data)
                l$setinverse(inverse)
                return(inverse)
        }
}
