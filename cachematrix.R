## This function creates a matrix object that can cache its inverse

## Cache the inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL
        set <- function(y) {
                x <<- y
                invx <- NULL
        }
        get <- function() x
        setInv <- function(inverse_x) invx <- inverse_x
        getInv <- function() invx
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Check if the inverse of a given matrix has been solved and cached
## If so, retrieve the inverse and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invx <- x$getInv()
        
        if (!is.null(invx)) {
                return(invx)
        }
        mx <- x$get()
        invx <- solve(mx)
        x$setInv(invx)
        invx
}
