## This function creates an object that stores the matrix and caches the 
## matrix inverse in order to use the inverse in repeated operations and avoid
## the recomputation of the inverse.

makeCacheMatrix <- function(x = matrix()) {
                    ## Create member that is the inverse of matrix x
                    xInv <- NULL
                    
                    ## Set function to set the matrix x
                    set <- function(y){
                      x <<- y
                      xInv <<- NULL
                    }
                    
                    ## Get function to get the matrix x
                    get <- function() x
                    
                    ## Set function to set the member xInv
                    setInv <- function(inverseMatrix) xInv <<- inverseMatrix
                    
                    ## Get function to get the member xInv
                    getInv <- function() xInv
                    
                    list(set = set, get = get, setInv = setInv, getInv = getInv)

}


## This function takes in as argument an object created by a makeCacheMatrix call

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xInv <- x$getInv()
        
        ## Check member in object x for a current value assigned to member xInv
        if(!is.null(xInv)){
          ## Member is non-NULL, return inverse
          message("Getting cached data...")
          return(xInv)
        }
        
        ## Member is NULL, calculate inverse
        data <- x$get()
        xInv <- solve(data)
        
        ## Set inverse
        x$setInv(xInv)
        
        ## Return inverse
        xInv
}
