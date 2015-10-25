## Tried to use descriptive variables to make things clearer

makeCacheMatrix <- function(theMatrix = matrix()) {
        ## I created a function that takes a matrix as an input.
        
        cachedInverse <- NULL
        ## So far the cached inverse is null. 
        
        setMatrix <- function(theInputMatrix) {
            
                theMatrix <<- theInputMatrix
                cachedInverse <<- NULL
        }
            ## Here I created the Set Matrix function, which inputs the matrix. 
        
        getMatrix <- function() theMatrix
        ## This allows us to call the matrix if we want to see it.
        
        setInverse <- function(inverse) cachedInverse <<- inverse
        ## This would allow us to theoretically set the inverse to whatever we want. 
        ## The double arrow allows this variable to be accessible in the global environment. 
        
        getInverse <- function() cachedInverse
        ## We can see the matrix inverse if we want. 
        
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
        ## This sets the functions and allows them to be called. 
}


        # Here we will allow ourselves to call the cached matrix data. 
cacheSolve <- function(x, ...) {
        cachedInverse <- x$getInverse()
        ## This just says, "Make the cached inverse whatever is currently stored as the inverse. 
        
        if(!is.null(cachedInverse)) {
                message("getting cached data")
                return(cachedInverse)
        }
        ## This asks if the inverse is null, if it isn't, return the current value of the cached inverse. 
        
        data <- x$getMatrix()
        ## Create a variable called data and set it to the current matrix.
        
        cachedInverse <- solve(data, ...)
        ## set the cached inverse variable equal to the actual calculated inverse of the matrix.
        ## This is the only line of the function doing actual mathemematical work. 
        
        x$setInverse(cachedInverse)
        ## This sets the value of the inverse to the cached inverse value. 
        
        return(cachedInverse)
        ## Shows the inverse. 
}




