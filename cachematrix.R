# Creates a list of functions to retrieve cache value of inverse of a matrix
# It is a constructor function
# Args : 
#       The matrix whose inverse is to be calculated and stored in a cache
#Returns: #  3 functions as a named list 
#               "getmatrix" gets the argument matirx
#               "setcachce" sets the inverse to cache
#               "getinverse" gets in inverse value if its already computed
makeCacheMatrix <- function(matrix = matrix()){
        
        # Set initial value of cache to NULL
        cache <-  NULL
        
        # Function to get the matrix
        getmatrix <- function() matrix
        
        # Value is assigned to the cache using <<- so the it can be used in 
        # a different env
        setcache <- function(inv){
                cache <<- inv
        }
        
        # Function to retrieve inverse of the given matrix
        getinverse <- function() cache
        
        
        
        # Returns all the functions as a list with specific names
        list (get = getmatrix, 
              setcache = setcache, 
              getinverse = getinverse)
}

# Funtion to compute the inverse of matrix or to retrieve already calculated inverse
# Args:
#       Name of the created list
#Retruns: 
#       Inverse of the matrix
cacheSolve  <- function(funList,...){
        # Check if the inverse is already calculated 
        inv <- funList$getinverse()  
        if (!is.null(inv)){
                message("Getting inverse from Cache")
                inv              
        } else {
                #Compute the inverese
                message("Computing inverse")
                matrix <- funList$get() # Retrieve the matrix using get func.
                inv <- solve(matrix)   # Find the inverse
                funList$setcache(inv)  # Send to the Glob env for caching.
                return(inv)             # Return the inverse of the matrix
                
        }       
}