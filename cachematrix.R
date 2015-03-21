# Creates a list of functions to retrieve cache value of inverse of a matrix
# It is a constructor function
makeCacheMatrix <- function(matrix = matrix()){
        
        # Set initial value of cache to NULL
        cache <-  NULL
        
        # Function to get the matrix
        getmatrix <- function() matrix
        
        # Function to retrieve inverse of the given matrix
        getinverse <- function() cache
        
        # Value is assigned to the cache using <<- so the it can be used in 
        # a different env
        setcache <- function(inv){
                cache <<- inv
        }
        
        # Returns all the functions as a list with specific names
        list (get = getmatrix, 
              setcache = setcache, 
              getinverse = getinverse)
}

# Funtion to compute the inverse of matrix or to retrieve already calculated 
# inverse value
cacheSolve  <- function(funList,...){
        # Call the function & Check if the inverse is already calculated 
        inv <- funList$getinverse()  
        if(!is.null(inv)){
                message("Getting inverse from Cache")
                inv              
        }
        
        # If it is not calculated before, compute the inverse
        else{   message("Computing inverse")
                matrix <- funList$get() # Retrieve the matrix using get func.
                inv <- solve(matrix)   # Find the inverse
                funList$setcache(inv)  # Send to the Glob env for caching.
                return(inv)             # Return the inverse of the matrix
                
        }       
}