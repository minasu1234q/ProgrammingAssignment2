## Create an object that contains the matrix and its inverse. 
## Calculate the inverse of this matrix once and save that value
## to the object so it can be retrieved again without calculation. 

## The makeCacheMatrix function returns a list that contains 
## setters and getters and the matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve returns the inverse inside of the object or
## calculate the inverse and set it. 

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        i
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
