## This function is to create a special "matrix", which contains
## functions to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse of the matrix
## 4) get the value of the inverse of the matrix

## The input to the function is a matrix without any element. The
## elements of the matrix will be set by the "set" function using
##

makeCacheMatrix <- function(x = matrix())
{
    inv <- NULL ## initial value of the inverse of the matrix is NULL
    set <- function(y)  ## fucntion to set the value of the matrix
    {
        x <<- y         ## y is an object in another environment
        inv <<- NULL    ## the inverse of the matrix is set to NULL
    }
    get <- function() x   ## function to get the value of the matrix
    setInverse <- function(inverse)   ## function to set the inverse
    {                                 ## of the matrix
        inv <<- inverse    ## inverse is from another environement
    }
    getInverse <- function() inv  ## function to get the inverse value
    
    list(set = set, get = get,        ## put the results of the four
    setInverse = setInverse,     ## functions together
    getInverse = getInverse)
    
}


## This function calculates the inverse of the special matrix created by
## the "makeCacheMatrix.R" function. It first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the inverse of the
## matrix and sets the value of the inverse in the cache via the setInverse
## function

cacheSolve <- function(x, ...)  ## Return a matrix that is the inverse of 'x'
{
    inv <- x$getInverse()     ## get inverse of the matrix from x
    if(!is.null(inv))           ## check if the inverse is NULL or not
    {                                       ## if the inverse is not NULL,
        message("getting cached data")    ## display the message, and
        return(inv)                         ## return the inverse value
    }
    data <- x$get()           ## if the inverse is NULL, get the matrix
    inv <- solve(data, ...)   ## and compute its inverse using "solve"
    x$setInverse(inv)         ## set the value of the inverse in the cache
    inv
    
}