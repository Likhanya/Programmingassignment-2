    makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    #set function for the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    #return matrix
    get <- function() {
        x
    }

    #set the inverse matrix 
    setInverse <- function(inv_Matrix){
        inverse <<- inv_Matrix
    } 

    #return the inverse matrix
    getInverse <- function(){
        inverse
    }

    #Here are all defined functions
    list(set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
    }


    cacheSolve <- function(x, ...) {

    inv_Matrix <- x$getInverse()

    #Has an inverse matrix already been calculated
    if (!is.null(inv_Matrix)){
    
       if ( identical( x$get() %*% inv_Matrix, inv_Matrix %*% x$get() ) ){ 
         print("getting cached data")
            return(inv_Matrix)
       }
    }

    #Calculating the inverse if inverse marix is null or matrix has changed and returning it
    data <- x$get()
    inv_Matrix <- solve(data, ...)
    x$setInverse(inv_Matrix)
    return(inv_Matrix)
    }
