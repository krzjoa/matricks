library(dplyr)
library(microbenchmark)

x <- matrix(1, 10000, 5)
z <- c(3,4,5,6,7)

nrm <- function(x, z){
  t(t(x)*z)
}

# lst <- nrm
# 
# x %o% z

smart_multiply(x, z)



microbenchmark(nrm(x, z), smart_multiply(x, z))

# res <- microbenchmark(nrm=nrm(x, z), sm=smart_multiply(x, z))
print(res)
