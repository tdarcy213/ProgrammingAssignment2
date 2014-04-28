## PROGRAM ASSIGNMENT #2
##
##         - CREATE A SPECIAL MATRIX OBJECT THAT CAN CACHE ITS INVERSE
##
##
## makeCacheMatrix creates a list of functions that will 
## 	- initialize an object where the inverse matrix will be cached
##	- get the matrix to be inversed
##	- cache the value of the inverse matrix
##	- get the cached inverse matrix

##
## HERE'S AN EXAMPLE OF THE FUNCTION CALL, USING A 5x5 UNIFORM DISTRIBUTION:
##
##	cacheSolve(makeCacheMatrix(matrix(runif(1:25),ncol=5)))
## 
## TEST THAT THE FUNCTION WORKED BY CALCULATING THE "IDENTITY MATRIX": m1%*%m2 !!!
##

makeCacheMatrix <- function(x = matrix()) {
        ##
        ## INITIALIZE INVERSE MATRIX OBJECT
        ##
        set <- function(x) {
                m2 <<- NULL
        }
##
## GET THE MATRIX TO BE INVERSED
##
get <- function() x
##
## CACHE THE INVERSE MATRIx
##
setinv <- function(inv) m2 <<- inv
##
## GET THE CACHED INVERSE MATRIX
##
getinv <- function() m2
##
## DEFINE A LIST OF THE FUNCTIONS DEFINED
##
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}
##
## cacheSolve INVOKES THE "SOLVE" FUNCTION TO CALCULATE THE INVERSE OF A MATRIX, THEN INVOKES THE FUNCTIONS
## DEFINED IN makeCacheMatrix TO CACHE THE INVERSE
##
cacheSolve <- function(x, ...) {
        ##
        ## TEST FOR THE EXISTENCE OF THE "m2" OBJECT - THIS IS WHERE THE INVERSE MATRIX WILL BE CACHED
        ## IF IT DOES NOT EXIST, INVOKE THE "set" FUNCTION TO INITIALIZE IT
        ##
        if(exists("m2") == FALSE)  {
                x$set(m2)
        }
        ##
        ## INVOKE THE "getinv" FUNCTION, TO RETRIEVE THE VALUE OF "m2" THAT HAS BEEN CACHED.  TEST TO DETERMINE
        ## IF THE CACHED VALUE IS "NULL"; IF NOT, THE INVERSE HAS BEEN CALCULATED PREVIOUSLY, SO NO FURTHER 
        ## PROCESSING IS DESIRED.
        ##
        m2 <- x$getinv()
        if(!is.null(m2)) {
                message("getting cached data")
                return(m2)
        }
        ##
        ## "m2" IS NULL, SO WE WILL EXECUTE THE STEPS TO CALCULATE THE INVERSE MATRIX
        ##
        ## FIRST, RETRIEVE THE MATRIX TO BE INVERSED, AND CACHE THAT INTO THE "m1" OBJECT
        ##
        data <- x$get()
        m1<<-data
        ##
        ## NEXT, INVOKE THE "solve" FUNCTION
        ##
        m <- solve(data, ...)
        ##
        ## NOW INVOKE "setinv" TO CACHE THE INVERSE MATRIX
        ##
        x$setinv(m)
        m
}
