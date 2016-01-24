#This script contains functions "makeCacheMatrix" and "cacheSolve". 
#'makeCacheMatrix' creates and updates CacheMatrix objects which store
#a matrix and its inverse as cached values.
#'cacheSolve' fetches from cache or computes the inverse of a CacheMatrix object
#then updates the value of the inverse within the CacheMatrix object

################################################################################
#makeCacheMatrix is a function that takes a matrix as input then
#creates a "Cachematrix" (list) object that stores functions that
#will store the matrix and its invers as cached values. 

makeCacheMatrix <- function(x){
    inverse = NULL  #  
    set.fn <- function(y) {
        x <<- y     #cache the  matrix value
        inverse <<- NULL #an update will reset the cached value of inverse
    }
    getRaw.fn <- function(){x} 
    setInverse.fn <- function(inverse){inverse <<- inverse}
    getInverse.fn <- function(){inverse}
    list(set = set.fn, getRaw = getRaw.fn,
         setInverse = setInverse.fn,
         getInverse = getInverse.fn)

}

################################################################################
#cacheSolve computes the inverse of a "cacheMatrix" object and updates
#the value of the inverse if the inverse is not cached, otherwise the inverse
#will be fetched from the cached value.
cacheSolve <- function(x, ...) {   
    inverse = x$getInverse()
    if(!is.null(inverse)) {   
        message("getting cached data")
        return(inverse)
    }
    
    #fetch the original matrix data from the "cacheMatrix" object
    #compute the inverse and update the "cacheMatrix" object in place
    data = x$getRaw() 
    inverse = solve(data, ...) 
    x$setInverse(inverse)      
    #return(inverse)
}
################################################################################
#Test case:
#myMatrix = matrix(c(4,3,3,2),nrow = 2) 
#xx = makeCacheMatrix(myMatrix)
#xx$getRaw()
#xx$getInverse()
#cacheSolve(xx)
#xx$getInverse()
################################################################################
