#Creates a list of functions to retrieve cache value of inverse of a matrix
#It is a constructor function
makeCacheMatrix <- function(matrix = matrix()){
        
        # initial value of cache is set to NULL
        cache <-  NULL
        
        #function to get the argumment matrix
        getmatrix <- function() matrix
        
        #Function to retrieve inverse of the given matrix
        getinverse <- function() cache
        
        #Value is assigned to the cache using <<- so the it can be used in 
        # a different env
        setcache <- function(inv){
                cache <<- inv
        }
        
        #returnig all the functions
        list (get = getmatrix, 
              setcache = setcache, 
              getinverse = getinverse)
}

# Funtion to compute the inverse of matrix or to retrieve already calculated 
#inverse value
cacheSolve  <- function(funList,...){
        # Calling the function & Checking if the inverse is already calculated 
        inv <- funList$getinverse()  
        if(!is.null(inv)){
                message("Getting inverse from Cache")
                inv              
        }
        
        #If it is not calculated before, computing the inverse
        else{   message("Computing inverse")
                matrix <- funList$get() #Retrieving the matrix using get func.
                inv <- solve(matrix)   #Finding the inverse
                funList$setcache(inv)  #Send it to the Glob env for caching.
                return(inv)             #returning the inverse of the matrix
                
        }       
}