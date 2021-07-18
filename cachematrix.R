## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix lets user set the value of a matrix within it's scope.
# It gets the value of matrix, cacheSolve then retrives the matrix from make..
# to check if the inverse exists in the cache if so, cache... returns the inverse
# If the inverse doesn't exist, cacheSolve inverses the matrix returns the same. 
# By looping the functions, we've effectively made sure that R doesn't keep finding
# inverses for matrices when it's done so in the past. 

# Essentially, makeCacheMatrix creates a list of functions that sets a matrix's value, gets the matrix's value then 
# sets and gets the inverse value for the matrix

makeCacheMatrix <- function(x = matrix()) {
        # make... creates a matrix with the data we feed it about the matrix through 
        # setmat and returns a list of functions that cacheSolve uses to get an 
        # inverted matrix from cache if any or set an inverted matrix in cache
        inverm<-NULL
        # setting the inverse matrix variable as null
        setmat<- function(y)
                # setting a matrix in the working environment        
        {
                x<<-y
                # assigning y to x forcefully as it's not x's environment
                inverm<<-NULL
                # setting inverm as null forcefully as it's not the working environment for it
        }
        getmat<-function() x
        # gets the value of the matrix that we input through setmat
        setinver<-function(inverse) inverm<<- inverse
        # inverts the matrix and stores it in cache for further need
        getinver<-function() inverm
        # gets the inverted matrix from the cache through 
        list(getmat=getmat, setmat=setmat, getinver=getinver, setinver)
        # return these functions to within the scope of make... 
}


## cacheSolve checks for preexisting inverses in the cache and returns it if there exists one. It gets the inverse from 
## the cache. If not, it solves for it and then sets the inverse value in the cache using setinverse. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # cacheSolve takes in the matrix 'x' 
        inverm <- x$getinver()
        # here getinver() is called and it assigns the inverse of 'x' on 'inver'
        if(!is.null(inverm)) 
        {
                # if 'inver' is not null i.e. inverse for 'x' available get into loop
                message("getting cached data")
                # prints the above message on the console
                return(inverm)
                # returns the 'inver' inverse matrix for 'x'
        }
        # loop closes
        mat <- x$getmat()
        # if there's no inverse value for 'x', cacheSolve asks get to assign 
        # x (from makeCacheMatrix's scope to here)
        
        inverm <- solve(mat, ...)
        # solves the inverse for mat and assign it to inverm
        x$setinver(inverm)
        # sets x's inverse as inverm in makeCacheMatrix's scope to store it in cache
        inverm
        # prints the inverse of the matrix x. 
}
