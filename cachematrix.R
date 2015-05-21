## This file contains two functions makeCacheMatrix() and cacheSolve. 
## makeCacheMatrix makes a 'special' type of matrix object that caches it inverse.
## cacheSolve checks whether the inverse already computed or not and return the 
## value accordingly.
## Assumption:  The matrix supplied is always invertible.

## makeCacheMatrix: creates a special "matrix" object that can cache its 
##                  inverse.
## x: matrix object, default value is matrix(). 
##    inverse of x is intended to be computed.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(newinverse) inverse <<- newinverse
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: computes the inverse of the special "matrix" returned by 
##             makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##      then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
       
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        mat <- x$get()
        inverse <- solve(mat, ...)
        x$setinverse(inverse)
        
        inverse
}

## External Sources: referred following links for sample invertible matrices:
##      http://www.sosmath.com/matrix/matinv/matinv.html
##      http://www.purplemath.com/modules/mtrxinvr2.htm
##      http://www.mathwords.com/i/inverse_of_a_matrix.htm
## Sample usage:
## 1.           splmat <- makeCacheMatrix(matrix(c(2,2,3,2),2,2))
##              cacheSolve(splmat) 
## 2.           splmat <- makeCacheMatrix(matrix(c(1,-1,1,2),2,2))
##              cacheSolve(splmat)
## Valid output of these above two call can be verified from the above said link.