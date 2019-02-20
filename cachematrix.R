## Put comments here that give an overall description of what your
## functions do

## function makeCacheMatrix can be passed an initial matrix to set matrix value or initiated with an empty
## value to.
## List is returned with 4 
## set(y) will reset the value of the cached matrix to whatever matrix is passed to this comment
## get() returns cached matrix.  if "NULL" then run set() passing a matrix, then run cacheSolve()
## setinv (inv) - cacheSolve() uses this function to cache the calculated inv value DO NOT USE OUTSIDE OF CacheSolve
## getinv() returns cached inv of saved function.  if "NULL" then run cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                if (class(y)=="matrix"){
                        x <<- y
                        inv <<- NULL
                } else {
                        print("Set Value needs to be type matrix")
                }

        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)  
        
}


## if an object of makeCacheMatrix$getinv() returns "NULL" then run cacheSolve passing the instance of the
## matrix you want to solve for the inverse of.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
