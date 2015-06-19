#The following function creates a matrix, set the matrix, set the matrix inverse and get the inverse

makeCacheMatrix <- function(x = matrix()){
        m<-NULL
        set<-function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The function returns the inverse of a matrix if it is already calculated else it calculate the inverse and return the 
##inverse matrix

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
