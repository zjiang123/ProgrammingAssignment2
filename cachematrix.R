## This assignment creates R function that is able to cache the inverse of a matrix


## makeCasheMatrix function set the value of the matrix, get the value of the matrix, set the value of the inverse, and get the value of the inverse
makeCacheMatrix <- function(matrix = matrix()) {
        #inverse Matrix
        invMatrix <- NULL #default value - NULL
        
        #set Matrix
        setMatrix <- function(y){
                matrix <<- y
                invMatrix <<- NULL
        }
        #getMatrix
        getMatrix <- function() matrix
        
        #set inverse matrix
        setInvMatrix <- function(inverseMatrix) invMatrix <<- inverseMatrix
        
        #getInvMatrix function: get inverse matrix
        getInvMatrix <- function() invMatrix
        
        list(setMatrix = setMatrix, getMatrix=getMatrix,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
}


## the cacheSolve function calculates the inverse of the special vector created with the above function. 

cacheSolve <- function(matrix, ...) {
        invMatrix <- matrix$getInvMatrix()
        
        #if invMatrix is not null, get the invMatrix from cache
        if (!is.null(invMatrix)){
                message("getting cached data")
                return (invMatrix)
        }
        
        #if it is null, calculate the inverse
        data <- matrix$getMatrix()
        inv <- solve(data, ...)
        matrix$setInvMatrix(inv)
        inv
}



