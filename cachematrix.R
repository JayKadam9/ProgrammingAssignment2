cacheSolve <- function(NewMatrix=matrix()) 
{
        #fetches the cached inverse, if already calculated
        cachedInverse <- NewMatrix$getInverseMatrix()
        
        #checks if the fetched value is NULL or not
        if(!is.null(cachedInverse)) 
        {
                message("Getting Cached Inverse for Matrix")
                return(cachedInverse)
        }
        
        #gets the value of NewMatrix, as it is not already cached
        fetchMatrix <- NewMatrix$getMatrix()
        
        #calculates the inverse of New Matrix
        cachedInverse <- solve(fetchMatrix)
        
        #caches the inverse matrix for future use
        NewMatrix$setInverseMatrix(cachedInverse)
        
        #returns value of inverse
        cachedInverse
}

makeCacheMatrix <- function(NewMatrix) 
{
        # Holds the cached Inverse of Matrix
        # Initially nothing calculated hence assigned NULL
        cachedInverse <- NULL
        
        #sets the Matrix
        setMatrix <- function(NewMatrix) 
        {
                NewMatrix <<- NewMatrix
                
                # Initially nothing calculated hence assigned NULL
                cachedInverse <<- NULL
        }
        
        #gets the Matrix
        getMatrix <- function()
        {
                NewMatrix  
        }
        
        #caches the Inversed Matrix for future use        
        setInverseMatrix <- function(cachedInverse) 
        { 
                cachedInverse <<- cachedInverse
        }
        
        #returns Inverse Matrix
        getInverseMatrix <- function()
        {
                cachedInverse
        }
        
        #returns List of named functions
        list(setMatrix = setMatrix, getMatrix = getMatrix,setInverseMatrix = setInverseMatrix,getInverseMatrix = getInverseMatrix)
        
        
}