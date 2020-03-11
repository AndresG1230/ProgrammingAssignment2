## Function that creates a special matrix object and caches its inverse
## You can use this code to run an example: 

##  testmatrix <- makeCacheMatrix(matrix(1:10, 2, 5))
##  testmatrix$get()
##  testmatrix$getInverse()  #This should return a Null value

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function computes the inverse of the special matrix object returned by the function makeCacheMatrix.
## if the inverse has already been calculated, it should retrieve the inverse.
## You can run the following code after running the first two test lines of code give for the function above, to test this function

## cacheSolve(testmatrix)
## testmatrix$getInverse  #This should now return the inverse of the test matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of the test matrix
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mtx <- x$get()
    inv <- solve(mtx, ...)
    x$setInverse(inv)
    inv
}