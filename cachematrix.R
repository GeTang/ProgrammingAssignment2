#The makeCacheMatrix function: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        n <- NULL  
        set <- function(y) {   
                x <<- y    
                n <<- NULL   
        }
        get <- function() x   #gets the value of the matrix
        setmatrix <- function(solve) n <<- solve  #solve(x) returns its inverse
        getmatrix <- function() n
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)  #creates a list to store the 4 functions
}



cacheSolve <- function(x = matrix(), ...) {  
        n <- x$getmatrix()  
        if(!is.null(n)) {   
                message("getting cached data") #if it has then provides this message
                return(n)
        }
        data <- x$get()
        n <- solve(data, ...) #computes the inverse of the input matrix
        x$setmatrix(n) #runs the setinverse function on the inverse to cache the inverse
        n #return the inverse
}
