## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverser <- NULL
        # 1. set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # 2. get the value of the matrix
        get <- function() x
        
        # 3. set the value of inverse of the matrix
        setinverse <- function(inverse) inverser <<- inverse
        
        # 4. get the value of inverse of the matrix
        getinverse <- function() inverser
        list(
                set=set,
                get=get,
                setinverse=setinverse,
                getinverse=getinverse
                
        )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        inverser <- x$getinverse()
        if(!is.null(inverser)) {
                ##If cached it is printed on screen
                message("getting cached data.")
                return(inverser)
        }
        data <- x$get()
        ## Calculate the inverse matrix and store into inverser variable
        inverser <- solve(data)
        
        ##store into setinverte matrix inverted calculated
        x$setinverse(inverser)
        inverser
}
