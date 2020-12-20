
#Name:Kelvin
#Function makeCacheMatrix will helps transform user input to a  matrix format, before setting the value of the matrix,
#get the value of the matrix, set the inverse Matrix and get the inverse Matrix. 
#The matrix object will than cache its own object. 

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  
  #set the value of the Matrix
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  getMatrix <- function() x                              
  setInverse <- function(inverse) invMatrix <<- inverse  
  getInverse <- function() invMatrix                     
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
  
}


## The function cacheSolve will brings the output of the previous function as an 
# input and checks  whether the inverse for of the  matrix from the previous function have any value  or not.
# In case inverse matrix from makeCacheMatrix((matrix) is empty, it will gets the original matrix data from 
# and set the inversible  matrix by using the solve function.
# For cases where the inverse matrix from the previous function has some value in it,
#it will returns a message  "getting cached data" 
#and the cached object
# While for cases where the inverse matrix from the previous function has no value  it will gets the original matrix data from 
# it will retrieve its original matrix value and this value will be set as the inversible  matrix by using the solve function.



cacheSolve <- function(x, ...) {
  
 
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {                      
    message("getting cached data")    
    return(invMatrix)                             
  }
  
  
  MatrixData <- x$getMatrix()                     
  invMatrix <- solve(MatrixData, ...)             
  x$setInverse(invMatrix)                         
  return(invMatrix)                               
}


#Test [2*2 Matrix]#
SampleMatrix <- matrix(1:4,2,2)
SampleMatrix

CacheMatrix <- makeCacheMatrix(SampleMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)




