## Below are two functions that are used to create a special object that stores a numeric matrix 
##and cache's its solved inverse matrix. 

##This function creates a special "matrix" object, which is
##containing functions to

##      1.  set the value of the matrix (x$set)
##      2.  get the value of the matrix (x$get)
##      3.  set the inverse to the cash (x$setinverse)
##      4.  get the inverse from the cash (x$getinverse)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # init inverse matrix var
        
        #set function: store matrix to the cash environment 
        set <- function(y) {
                x <<- y             #set x in "cash" environment
                inverse <<- NULL    #set inverse in "cash" environment
        }
        
        #get matrix from cash environment
        get <- function() x
        
        #set inverse matrix to the cash environment
        setinverse <- function(inverse) inv <<- inverse
        
        #get inverse matrix from cash environment
        getinverse <- function() inv
        
        #return a list containing the functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by the function makeCache Matrix
## 
cacheSolve <- function(x, ...) {
        inv <- x$getinverse() # get NULL or inverse matrix
        
        if(!is.null(inv)) {
                #return inverse matrix from cash environment         
                message("getting cached data")
                return(inv)
        }
        
        #get matrix and solve inverse
        data <- x$get()
        inv <- solve(a=data,...)
        
        #set inverse matrix to cash environment and return inverse matrix
        x$setinverse(inv)
        inv
}
