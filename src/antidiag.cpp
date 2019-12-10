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

  // NumericMatrix matrix(3, 3);
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
  for(int i=0; i<n_row; i++){
    matrix(i, n_row - 1 - i) = x(i);
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
//' @examples
//' antidiag(diag(3))
//' @export
// [[Rcpp::export]]
RObject antidiag(RObject x = NumericVector::create(1),
                 Nullable<NumericVector> nrow = R_NilValue,
                 Nullable<NumericVector> ncol = R_NilValue){
  if(Rf_isMatrix(x)){
    return antidiagonal((NumericMatrix) x);
  } else if(Rf_isNumeric(x)){
    return antidiag_matrix((NumericVector) x, nrow, ncol);
  } else {
    return R_NilValue;
  }
}


