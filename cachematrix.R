## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a special matrix containing
## a function to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) sets the inverse value of the matrix
## 4) returns the set inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) { ## when I want to change the matrix
                x <<- y ## substitutes the matrix x with y (input) 
                ## in main function
                
                s <<- NULL ## restores the value of the inverse 
                ## to null
        }
        
        get <- function() x ## returns the matrix x stored in the 
        ## main function
        setinverse <- function(solve) s <<- solve ## store the value
        ## of the input in variable s into the main makeCacheMatrix 
        ## function
        getinverse <- function() s ## returns the stored value of the
        ## input in variable s
        
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
        ## stores the 4 functions (get,set, setinverse, getinverse)
        ## in the getCacheMatrix function, so that any object assigned
        ## to the main function has all 4
        
}

## The cacheSolve function calculates the inverse of the special matrix
## created with makeCacheMatrix. It first checks to see if the inverse
## has already been calculated. If this is the case, it retrieves the
## inverse from the cache and skips the computation. If it has not
## already been calculated, then it performs this and sets the value
## of the inverse in the cache.

cacheSolve <- function(x, ...) {
        ##Return a matrix that is the inverse of x
        s <- x$getinverse()
        
        if(!is.null(s)) { ## verifies if the value s, stored with
                ## getinverse exists and is not NULL. 
                
                message("getting cached data")
                return(s) 
                ## If value s exists, returns message and the value 
                ##that represent the inverse
        } 
        
        ## If value s was not previously stored, then the following:
        
        data <- x$get() ## retrieves matrix stored with makeCacheMatrix
        
        s <- solve(data, ...) ## calculates the inverse of the matrix
        
        x$setinverse(s) ##stores it in the object generated and
        ## assigned with makeCacheMatrix
        
        s
}
