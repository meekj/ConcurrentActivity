// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// concurrentActivity
NumericVector concurrentActivity(int outlen, NumericVector StartSecond, NumericVector Duration);
RcppExport SEXP _ConcurrentActivity_concurrentActivity(SEXP outlenSEXP, SEXP StartSecondSEXP, SEXP DurationSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type outlen(outlenSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type StartSecond(StartSecondSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Duration(DurationSEXP);
    rcpp_result_gen = Rcpp::wrap(concurrentActivity(outlen, StartSecond, Duration));
    return rcpp_result_gen;
END_RCPP
}
// concurrentEstimatedThroughput
NumericVector concurrentEstimatedThroughput(int outlen, NumericVector StartSecond, NumericVector Duration, NumericVector Bytes);
RcppExport SEXP _ConcurrentActivity_concurrentEstimatedThroughput(SEXP outlenSEXP, SEXP StartSecondSEXP, SEXP DurationSEXP, SEXP BytesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type outlen(outlenSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type StartSecond(StartSecondSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Duration(DurationSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Bytes(BytesSEXP);
    rcpp_result_gen = Rcpp::wrap(concurrentEstimatedThroughput(outlen, StartSecond, Duration, Bytes));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_hello_world
List rcpp_hello_world();
RcppExport SEXP _ConcurrentActivity_rcpp_hello_world() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(rcpp_hello_world());
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_ConcurrentActivity_concurrentActivity", (DL_FUNC) &_ConcurrentActivity_concurrentActivity, 3},
    {"_ConcurrentActivity_concurrentEstimatedThroughput", (DL_FUNC) &_ConcurrentActivity_concurrentEstimatedThroughput, 4},
    {"_ConcurrentActivity_rcpp_hello_world", (DL_FUNC) &_ConcurrentActivity_rcpp_hello_world, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_ConcurrentActivity(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
