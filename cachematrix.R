## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, then the cacheSolve retrieves the inverse from the cache

## makeCacheMatrix function creates a list of three functions
## First function returns the value of the original matrix
## Second function sets the value of the solution to inv
## Third function returns the value of the inverse matrix which has been written to inv

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        get <- function() x
        setinverse <- function(solution) inv <<- solution
        getinverse <- function() inv
        list(get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## casheSolve function returns a matrix that is the inverse of 'x'
## If 'x' is written by makeCacheMatrix, casheSolve will do following:
## First, it accesses the object 'x' and gets the value of the inverse matrix
## Second, if inverse matrix was already cached, sends the message "getting cached data"
## to the console and return the inverse matrix
## If inverse matrix wasn't cached, and inv is NULL, casheSolve will calculate the inverse matrix
## and store the calculated value in 'x'
## At last, casheSolve returns the inverse matrix

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
