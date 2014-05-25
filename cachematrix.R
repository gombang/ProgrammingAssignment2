## This file contains two functions
## the first, encapsulate matrices in a convenient function
## the second, read the encapsulated matrices and solve for the inverse


##  makeCacheMatrix function encapsulate the matrix in a list
##  the input is a matrix, returns a list that contains:
##  1. set: a function to set/change the matrix contained in the list
##  2. get: a function to fetch the matrix contained in the list
##  3. setinverse: a function to save the inverse of the matrix. It will set NULL if the inverse hasn't been calculated yet
##  4. getinverse: a function to fetch the inverse of the matrix. It will return NULL if the inverse hasn't been calculated yet
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inv <<- i
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve function basically calculates the inverse of a matrix
## accepted input: the list created by makeCacheMatrix, which encapsulates a matrix
## returns the inverse in the form of a matrix (not a list in the format created by makeCacheMatrix)
## first it will check whether there is a cached inverse
## if there is one, it will return the cache
## otherwise it calculates the inverse using function solve() and save the solution to a cache
## and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}


