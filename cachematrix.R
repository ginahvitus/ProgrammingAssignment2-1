## The purpose of this of function is to generate
## and solve special matricies and their inverses
## using the two function makeCacheMatrix which
## will provide methods for getting/setting matrix
## and inverse values

## makeCacheMatrix will provide the value
## entries for the cacheSolve function
## specifically determining the get/set
## functions for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
    ## Initially assigning 'NULL' to inverse
    inverse <- NULL 	
    
    set_matrix <- function(y) {		
    
      x <<- y 
      
      ## Setting the matrix 'x'
      inverse <<- NULL
    }
    ## Returning matrix 'x'
    get_matrix <- function() x 		 
    
    ## Cache the value of the inverse 
    set_inverse <- function(solve) inverse <<- solve 
    
    ## Returning inverse
    get_inverse <- function() inverse 		           
    
    list(set_matrix = set_matrix, get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
  
  

}


## This function solves for the inverse of 
## the cachedMatrix, the product of the
## makeCacheMatrix function

cacheSolve <- function(x, ...) {
  
  ## Return the inverse of x    
  
  ## Getting and store that inverse
  inverse <- x$get_inverse()			    
  
  
  ## Checking inverse
  if(!is.null(inverse)) {					     
    message("getting cached data")			
    return(inverse)
  }
  
  ## Getting Matrix
  data <- x$get_matrix()		
  
  ## store result of solve in inverse
  inverse <- solve(data, ...)		        
  x$set_inverse(inverse)

  ## return result
  inverse 						                 
  
  
}
