## makeCacheMatrix stores the matrix that is input as an argument.  It also
## stores the inverse of the matrix, created by cacheSolve, such that subsequent
## calls to cacheSolve retrieve the chached inverse matrix rather than 
## resolving.

## Generates a list of functions that set and get the matrix as well as set and 
## get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL                             #initialize inverse so it can be written to later
    set <- function(y) {
        x <<- y                                 #Assigns y (the fn input) to x (x is already defined in the fn input)
        inverse <<- NULL                        #Resets inverse
    }
    get<- function() x                          #Returns x, the original input
    setinv <- function (inv) inverse <<- inv    #Assigns inv to inverse
    getinv <- function () inverse               #Returns inverse
    list(set = set, get = get,                  #Returns the list of functions
         setinv = setinv, 
         getinv = getinv)
}


## Determins if the matrix inverse has already been solved for.  If it has been
## solved, it returns the cached matrix.  If it hasn't been sovled for, it 
## solves it and caches the result.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getinv()                       #Assigns the inverse from the list of functions previously-defined
    if(!is.null(inverse)) {                     #if inverse already contains a matrix...
        message("getting cached data")          #Print message
        return(inverse)                         #Return the inverse matrix as the function output, ending the fn here
    } 
    data <- x$get()                             #If inverse was empty, assign data to the matrix input to makeCacheMatrix
    inverse <- solve(data, ...)                 #solve for the inverse of data and assign it to inverse
    x$setinv(inverse)                           #send inverse to the setinv function to be stored in cache
}
