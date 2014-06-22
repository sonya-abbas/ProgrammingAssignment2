## This pair of functions cache the inverse of a matrix and retreive it if it's exist 
## or calculate it 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x['getinverse()']
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## Return the original matrix in order to calculate the inverse
        data <- x['get()']
        ## Solve() function return the inverse of data matrix 
        inv <- solve(data) %*% data
        x['setinverse(inv)']
        inv
}

