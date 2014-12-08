## Put comments here that give an overall description of what your
## functions do

#This function take a square matrix as input, then create a list of functions.
#At the beginning im (=inverse matrix) is equal to NULL.

#Set_mat function set m<-NULL and is able to change matrix x with y value.

#Get_mat provides the input matrix

#Set_im set the inverse matrix. This is the key step because
#the << operator (below in HERE line) changes the value of m globally
#so the NULL value that m had at the beginning is substituted by im.
#We can say that the operator << (in m<<-im) has the power to change the m value
#"GLOBALLY", so that the functions "get_mat,get_im,set_mat" now see this m value and
#no NULL value for m.

#Get_im gets the inverse matrix

#TIP to understand better: if you are a little bit familiar with "Object Programming"
#m is like as attribute and methods are the functions below

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL #HERE
    set_mat <- function(y) {
      x <<- y
      m <<- NULL
    }
    get_mat <- function() x
    set_im <- function(im) m <<- im
    get_im <- function() m
    list(set_mat = set_mat, get_mat = get_mat,
         set_im = set_im,
         get_im = get_im)
  }


## Write a short comment describing this function
#This function try to get the inverse matrix. If it has not been calculated yet
#(and in this case the if block is not executed), the inverse matrix is calculated (im)
#and im is set as global variable in makeCacheMatrix function (above).
#If the im is already calculated, the if block is executed and the function exit
#when it hits "return(im)". In this case im is no calculated but retrieved by 
#makeCacheMatrix function!


cacheSolve <- function(x, ...) {
    im <- x$get_im()
    if(!is.null(im)) {
      message("getting cached data")
      return(im)
    }
    data <- x$get_mat()
    im <- solve(data, ...)
    x$set_im(im)
    im
  }
