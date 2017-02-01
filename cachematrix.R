## 
## define a 'pseudo matrix' object which caches it's inverse and 
## define cacheSolve function returning the inverse of a 'pseudo matrix'
## usage 
## a<-makeCacheMatrix(anInversibleMatrix)
## or 
## a<-makeCacheMatrix()
## a$set(anInversibleMatrix)
## inversea<-cacheSolve(a)
## subsequent calls to cacheSolve will use the inverse stored in our object.
##
## rderbier Feb 2017


## makeChacheMatrix : return our special object hodling matrix value, inverse cache and setter and getter functions

makeCacheMatrix <- function(x = matrix()) {
        xinverse <- NULL
        ## this current environment is the parentEnvironment functions set,get,setinverse,getinverse
        ## x, xinverse are defined in this parentEnvironment
        ## notet the use of double arrow opertor in the functions to set parent environment data
        ##
        ##  set : simply set x at parent level and clear the cache inverse.
        set <- function(y) {
                x <<- y
                xinverse <<- NULL
        }
        ## get : return x (defined at parent environment)
        get <- function() x
        setinverse <- function(inverse) xinverse <<- inverse
        getinverse <- function() xinverse
        ## last expression is the one returned
        ## our object is a list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve is using our pseudo matrix to retrieve or compute the inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                ## no need to compute, just return the cached value.
                return(inverse)
        }
        ## not in the cache : compute the inverse and save it
        data <- x$get()
        ## !!!! we are assuming that data is an inversible matrix !
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
