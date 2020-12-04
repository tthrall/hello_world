### 
## 
# digit_tally()
#   Count occurrences of each decimal digit of integer n > 0.
#   Required: stringr::
## 
### 
digit_tally <- function(n) {
  str_count(
    as.character(n), 
    as.character(0:9))
}


### 
## 
# num_digits()
#   Number of decimal digits of integer n > 0.
#   Required: digit_tally()
## 
### 
num_digits <- function(n) {
  sum(digit_tally(n))
}


### 
## 
# num_u_digits()
#   Number of unique decimal digits of integer n > 0.
#   e.g. u_digits(10001) = {0, 1}.
#   Required: stringr::
## 
### 
num_u_digits <- function(n) {
  sum(! is.na(str_extract(
    as.character(n), 
    as.character(0:9))) )
}


### 
## 
# sum_d_reps()
#   Sum the number of additional occurrences 
#   of each decimal digit of integer n > 0.
#   Required: num_digits(), num_u_digits()
## 
### 
sum_d_reps <- function(n) {
  num_digits(n) - num_u_digits(n)
}


### 
## 
# get_p_r_tbl()
#   Return tibble {k, p, r} with 
#     k = row index 
#     p = segment of prime numbers 
#     r = sum_d_reps(p) 
#   Required: numbers::
## 
### 
get_p_r_tbl <- function(
  n1,        # 1st Primes() argument
  n2 = NULL  # 2nd Primes() argument
) {
  p_vec <- as.integer(Primes(n1, n2))
  
  r_vec <- vector(mode = "integer")
  for (k in 1:length(p_vec)) {
    r_vec[k] <- sum_d_reps(p_vec[k])
  }
  
  return(tibble(
    k = 1:length(p_vec), 
    p = p_vec, 
    r = r_vec))
}


## 
# EOF 
## 