#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
//' Compute the ensemble mean and standard deviation
//'
//' @param ens A 3d array with the third dimension as the ensemnble member
//'
//' @return A list with \code{ens_sd}, the ensemble standard deviation and
//'   \code{ens_mean}, the ensemble mean
//'
//' @examples
//' my_file <- get_data_filename(20180101, 0)
//' t2m <- read_members(my_file$filepath, my_file$filename, 9, "T2m", lead_time = 0)
//' my_mean_and_sd <- ens_mean_and_sd(t2m)
//' @export
// [[Rcpp::export]]
Rcpp::List ens_mean_and_sd(arma::cube ens) {
	arma::mat ens_mean = arma::mean(ens, 2);
	arma::mat ens_sd(ens.n_rows, ens.n_cols, arma::fill::zeros);
	for (int i = 0; i < ens.n_slices; i++) {
	   ens_sd += pow(ens.slice(i) - ens_mean, 2.0);
	}
	ens_sd = sqrt(ens_sd / (ens.n_slices -1));
	return Rcpp::List::create(Rcpp::Named("ens_sd")   = ens_sd,
  	                        Rcpp::Named("ens_mean") = ens_mean);
}
