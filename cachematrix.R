## This is a pair of functions that cache the inverse of a matrix.
## Matrix inversion is usually a costly computation and these functions are 
## beneficial in that they cache the inverse of a matrix rather than it computing repeatedly


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {        
                
                inv <- NULL                        
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setinv <- function(solve) inv <<- solve
                getinv <- function() inv
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached matrix inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}


## this can be used for test data and to test the pair of functions
y<-c(1, 2, 3, 0, 1, 4, 5, 6, 0)
y_mat<-matrix(y, 3, 3)
cacheSolve(makeCacheMatrix(y_mat))



