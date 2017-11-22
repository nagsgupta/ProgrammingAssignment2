## Using the solve function to get the inverse of a non singular matrix
## solve(a, b, ...) where a is the matric for which you want the inverse and 
## b is the corresponding identity matrix

## This function is creating the getters and setters for cache matrix

makeCacheMatrix <- function(x = matrix()) {
        inversematrix <<-  NULL
        set <- function(y){
                x <<- y
                inversematrix <<- NULL
        } 
        get <- function() x
        setinverse <- function(solve) inversematrix <<- solve
        getinverse <- function() inversematrix
        list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)

}


## This function checks the cache if the inverse of the matrix is available
## or not. If not then it creates a new inverse else use the cashed data

cacheSolve <- function(x, ...) {
        inversematrix <- x$getinverse()
        if(!is.null(inversematrix)){
                message("getting cached data")
                return(inversematrix)                
        }
        matrixdata <- x$get()
        inversematrix <- solve(matrixdata,diag(nrow(matrixdata)))
        x$setinverse(inversematrix)
        inversematrix
        ## Return a matrix that is the inverse of 'x'
}
