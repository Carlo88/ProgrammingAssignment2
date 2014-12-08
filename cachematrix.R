## Put comments here that give an overall description of what your
## functions do

#This function takes a square matrix as input, then create a list of functions (below explained)
#At the beginning inv_matrix is equal to NULL.

#Set_matrix function set inv_matrix<-NULL and if needed change the matrix x with y value.

#Get_matrix provides the original matrix (mat)

#Set_invmat set the inverse matrix. This is the key step because
#the << operator (in inv_matrix <- NULL) changes the value of inv_matrix globally
#so the NULL value that inv_matrix had at the beginning is substituted by inv_matrix.
#We can say that the operator <<  change the inv_matrix value
#"GLOBALLY", so that the functions below "get,set..." now can see this inv_matrix value.


#Get_im gets the inverse matrix

#TIP to understand better: if you are a little bit familiar with "Object Programming"
#inv_matrix is the object attribute and objects methods are the 4 functions below

makeCacheMatrix <- function(mat = matrix()) {
    inv_matrix <- NULL #set inverse matrix as NULL
    
    set_matrix <- function(y) {
      mat <<- y
      inv_matrix <<- NULL
    }
    
    get_matrix <- function() mat #get value of original matrix
    set_invmat <- function(m) inv_matrix <<- m #Set a value for inverse matrix
    get_invmat <- function() inv_matrix #Retrieve inverse matrix
    
    list(set_matrix = set_matrix, get_matrix = get_matrix,
         set_invmat = set_invmat,
         get_invmat = get_invmat) #Create a list of functions that will be used by cacheSolve function 
  }


## Write a short comment describing this function
#This function get the inverse matrix: if it has not been calculated yet
#(and in this case the if block is not executed), the inverse matrix is calculated
#and inv_matrix is set using "get_invmat" and this value modify the inv_matrix value in makeCacheMatrix function above.

#If the inv_matrix is already calculated, the if block is executed and the function exits
#when it hits "return(inv_matrix)". In this case inv_matrix is no calculated again but it is retrieved by 
#makeCacheMatrix function that stored it in the last execution.


cacheSolve <- function(x, ...) {
    inv_matrix <- x$get_invmat() #Try to get inv_matrix value...
    if(!is.null(inv_matrix)) { #if block executed if inv_matrix is already calculated.
      message("getting cached data")
      return(inv_matrix) #Exit the function without any calculation
    }
    data <- x$get_matrix() #If inv_matrix was NULL...
    inv_matrix <- solve(data, ...) #Calculate inv_matrix
    x$set_invmat(inv_matrix) #Set inv_matrix with set_invmat function
    inv_matrix #Print inverse matrix
  }
