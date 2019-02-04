// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// read_brenda_file
std::vector<std::string> read_brenda_file(const std::string& filepath);
RcppExport SEXP _brendaDb_read_brenda_file(SEXP filepathSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::string& >::type filepath(filepathSEXP);
    rcpp_result_gen = Rcpp::wrap(read_brenda_file(filepath));
    return rcpp_result_gen;
END_RCPP
}
// separate_entries
std::vector<std::vector<std::string>> separate_entries(const std::vector<std::string>& lines);
RcppExport SEXP _brendaDb_separate_entries(SEXP linesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<std::string>& >::type lines(linesSEXP);
    rcpp_result_gen = Rcpp::wrap(separate_entries(lines));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_brendaDb_read_brenda_file", (DL_FUNC) &_brendaDb_read_brenda_file, 1},
    {"_brendaDb_separate_entries", (DL_FUNC) &_brendaDb_separate_entries, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_brendaDb(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}