## makeCacheMatrix creates a special matrix object and caches its inverse
## cacheSolve returns an inverse of the matrix object if it exists in the cache,
## otherwise it will calculate the inverse of the special matrix objet that is returned by makeCachematrix.

## Receives a special "matrix" object and stores it in the global environment

makeCacheMatrix <- function(x = matrix()) {
    ##initialize the inverse matrix
    invMatrix <- NULL
    
    #define the set function for the matrix
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    
##define the return matrix object by defining it as get
    get <- function() {
        x 
    }
    
##assigning the inverse matrix 
    setInvMatrix <- function(invM){
        invMatrix <<- invM 
    } 
    
##defining the function for returning the inverse matrix
    getInvMatrix <- function(){
        invMatrix 
    }
    
##create a list for future call within the environment
    list(set = set,
         get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}


## A function that checks if the inverse object exists
## and returns the inverse matrix from the cache if it already exists.
## Otherwise, the inverse of the matrix is calculated 

cacheSolve <- function(x, ...) {
##retrieve matrix from makeCacheMatrix
        invM <- x$getInvMatrix()
    
## check if inverse matrix is already saved in cache
    if (!is.null(invM)){
## retrieve inverse matrix from cache and skip the computation. 
            print("getting cached data")
            return(invM)
    }
    
## If inverse matrix is not stored in cache, it is calculated 
    data <- x$get()
    invM <- solve(data, ...)
    
## sets the value of the inverse in the cache via the setinv function.
    x$setInvMatrix(invM)
    
## Return a matrix that is the inverse of 'x'
    return(invM)
}
