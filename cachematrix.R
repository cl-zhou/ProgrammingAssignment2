## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object, which is a list of functions
## to get the matrix, set the matrix, get the inverse of the matrix
## and cache the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(sol) inv <<- sol
    getinverse <- function() inv
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
    
}


## Write a short comment describing this function

cachesolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## if the inverse has been cached, read the cache
    inv <- x$getinverse()
    if(!is.null(inv)){
        message('getting cached data')
        return(inv)
    }
    ## if not, calculate the inverse, return and cache the result
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    return(inv)
    
}
