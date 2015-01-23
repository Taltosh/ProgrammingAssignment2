##  Overview: Coursera R Programming assignment
##      This pair of functions, makeCacheMatrix() and cacheSolve() can be used to create a set of functions for a 
#       given square matrix which is invertible, that will:
#           Using makeCacheMatrix() create a child environment containing the initial square matrix and, once calculated, 
#           the value of the inverse matrix. This function will typically be assigned into a variable which will then be used  
#           as an argument to cacheSolve()
#           Using cacheSolve(), where the argument for the function is the variable created my makeCacheMatrix, return the inverse 
#           matrix. This function works by accessing the value of the inverse matrix within the child environment created by the 
#           makeCacheMatrix function, and returning that result. If the inverse has not previously been requested, this is 
#           calculated, stored for future requests, then the value returned. 
#
#   Notes on completing this assignment
#   -----------------------------------
#
#   At first glance, I assumed that the task was to create an index of cached values which would be looked up using a  
#   square matrix as an argument. Subsquent reading - particularly the community discussions, testing and working through 
#   the examples helped make clear that
#   
#   (a) I was incorrect, there is no index type lookup
#       Refs:   
#               https://class.coursera.org/rprog-010/forum/thread?thread_id=364 (Hussain Boxwala post)
#               https://class.coursera.org/rprog-010/forum/thread?thread_id=364#post-2451 (J R Jasperson reply to Hussain's post, 
#                   and several follow up replies)
#
#   (b) For each matrix (or vector in the original) the 'make' command creates a variable which utilises a 'pocket' environment where 
#       the value of the original matrix and the inverse result from solve() is stored
#       This, if you will, is the 'clever bit' with good explanations here on how '<<-' works and enables values set within
#       the 'pockets' to be used within the main part of the program or function that is using the matrix/inverse caching.
#       Refs:
#               http://r.789695.n4.nabble.com/What-does-the-operator-mean-tp3466657p3466737.html (Thomas Lumley-2, particularly 
#                       the comment about 'Wrong and Evil' uses of the superassignment operator)
#               http://adv-r.had.co.nz/Environments.html
#               https://class.coursera.org/rprog-010/forum/thread?thread_id=364#post-4789 (Philip Marsden)
#
#       NOTE: The makeCacheMatrix needs to be saved to a variable - otherwise the instance of the cached matrix
#       (and the cached value/environment it's saving) is lost i.e. this function is most effective within a function or script, 
#       not standalone on the command line. similarly, the cacheSolve function works with values created by the makeCacheMatrix, not 
#       other variables. 
#
#       Once a cached value has been stored using makeCacheMatric
#       e.g. 
#           a_matrix <- matrix(c(1:4), nrow=2)
#           #this simple square matrix is invertible
#           a_matrix_cache <- makeCacheMatrix(a_matrix)
#       
#       # now to get the inverse of the matrix
#       
#           cacheSolve(a_matrix_cache)
#           # the first time this is called, the inverse is calclated by solve(a_matrix), stored and returned. 
#           # subsequent calls return the cached or stored value      
#
#   (c) Within any usage for these functions, the script using them will take care of the process for calling each 
#       cached result using the variable created from the 'make' function - the assignment doesn't require this to be developed
#
#   (D) As per comments on the community pages, I'm not sure of the value or purpose of the $set function, it seems odd 
#       that you would change the matrix value in this way and could lead to errors (e.g. if the matrix value was changed  
#       and the inverse not recalculated.
#       Refs:
#               https://class.coursera.org/rprog-010/forum/thread?thread_id=364#comment-2103        
#
## Description: makeCacheMatrix()
#   with an input of a square matrix which is invertible, create a set of functions which can be used to initially store, then return
#   the cached value of the inverse matrix via the cacheSolve() function.
#

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    #   The '<<-' assignment looks for the variable the value is being assigned to in the immediate environment, 
    #   successive parent environments until the global environment is reached and, if the variable does not exist, 
    #   creates the variable in the global environment.
    
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list (set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse
          )
}

## Description: cacheSolve()
#   Using a variable created with  makeCacheMatrix(), return the inverse of a matrix supplied as the argument to makeCacheMatrix. 
#   If the inverse has previously been requested, return the stored result, otherwise calculate the inverse matrix, store this in the 
#   local environment and return the result.  
#

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    #   retrieve the stored value for the inverse matrix
    inv <- x$getinverse()

    
    #   Test whether the inverse has been created yet

    if(!is.null(inv)) {

    #   message("getting cached data")
    #   This message not really required, as it really only helped explain how makeVector/cachemean functions worked in the example

    #   Yes - inverse has been calculated previously, so returning saved (i.e.cached value previously calculated) result
        
        return(inv)
    }

#   variable inv is NULL, so the inverse of the initial matrix has not yet been calculated. So we calculate it now: 

    data <- x$get()
    inv <- solve(data, ...)

#   thenn store the calculated inverse of the initial matrix for the next time the inverse is required

    x$setinverse(inv)

#   return inverse of the inital matrix 

    inv
}
