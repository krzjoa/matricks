#include <Rcpp.h>
using namespace Rcpp;

// Auxilliary funcion to extract antidiagonal from a matrix
NumericVector antidiagonal(NumericMatrix x){
  int nrow = x.nrow();
  int ncol = x.ncol();

  NumericVector num_vec (nrow);

  for(int i=0; i<nrow; i++){
    num_vec(i) = x(i, nrow - 1 - i);
  }
  return num_vec;
}

// antidiag(5:1, 3, 4)
NumericMatrix antidiag_matrix(NumericVector x,
                              Nullable<NumericVector> ncol = R_NilValue,
                              Nullable<NumericVector> nrow = R_NilValue){

  int n_row = 1;
  int n_col = 1;

  if(x.length() > 1){
    n_row = x.length();
    n_col = x.length();
  }

  if(!Rf_isNull(nrow)){
    n_row = (int)((NumericVector) nrow)(0);
  }

  if(!Rf_isNull(ncol)){
    n_col = (int)((NumericVector) ncol)(0);
  }

  NumericMatrix matrix(n_row, n_col);

  // Insert values
  for(int i=0; i < n_row; i++){
    int index = i % x.length();
    matrix(i, n_row - 1 - i) = x(index);
  }

  return matrix;
}

//' @name antidiag
//' @title Matrix antidiagonals
//' @description
//' Extract or replace the antidiagonal of a matrix,
//' or construct a antidiagonal matrix.
//' @param x matrix, vector or 1D array, or missing.
//' @param ncol number of columns (optional; when x is not a matrix)
//' @param nrow number of rows (optional; when x is not a matrix)
//' @param value either a single value or a vector of length equal to that of the current antidiagonal.
//' Should be of a mode which can be coerced to that of x.
//' @examples
//' # Extracting antidiag
//' antidiag(diag(3))
//' # Creating antidiagonal matrix
//' antidiag(7, 3, 3)
//' antidiag(1:5, 3, 3)
//' # Assigning antidiagonal
//' mat <- matrix(0, 3, 3)
//' antidiag(mat) <- c(3, 4, 5)
//' mat
//' @export
// [[Rcpp::export]]
RObject antidiag(RObject x = NumericVector::create(1),
                 Nullable<NumericVector> nrow = R_NilValue,
                 Nullable<NumericVector> ncol = R_NilValue){
  // TODO: Behaviour when nrow != ncol
  if(Rf_isMatrix(x)){
    return antidiagonal((NumericMatrix) x);
  } else if(Rf_isNumeric(x)){
    return antidiag_matrix((NumericVector) x, nrow, ncol);
  } else {
    return R_NilValue;
  }
}

//' @rdname antidiag
//' @export
// [[Rcpp::export(name = `antidiag<-`)]]
RObject antidiag_assign(NumericMatrix x, NumericVector value){
  // https://stackoverflow.com/questions/58877742/is-there-any-way-in-which-to-make-an-infix-function-using-sourcecpp
  // TODO: Funtions behaviour for long vectors
  int n_row = x.nrow();
  int n_col = x.ncol();

  for(int i=0; i < n_row; i++){
    int index = i % value.length();
    x(i, n_row - 1 - i) = value(index);
  }

  return x;
}


