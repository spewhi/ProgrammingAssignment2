## The first function creates a class that stores a matrix as its object. The second function 
## checks if theres a value for the inverse already stored in the global environment and 
## if not computes it and stores it.


## MakeCacheMatrix creates a class object that is a set of functions that store attributes 
## of a matrix in the global environment 

makeCacheMatrix <- function(x = matrix()) {
        m<- NULL
        
        set<-function(y) {
                x <<- y
                m <<- NULL
        }
        get<- function() x
        setmatrix<- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)

}



## cacheSolve takes an argument x that is of the class makeCacheMatrix and first checks to see if there is a value 
## already associated with it in the environment.If there is it returns the stored value and 
## if there isnt it calculates the inverse of the matrix and stores it.
cacheSolve <- function(x, ...) {
        m<- x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<- x$get()
        m<- solve(data, ...)
        x$setmatrix(m)
        m
}
