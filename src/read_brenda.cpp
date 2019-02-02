#include <Rcpp.h>
#include <fstream>
#include <sstream>
#include <regex>

using namespace Rcpp;

//' @export
// [[Rcpp::export]]
StringVector read_brenda_file(const std::string &filepath) {
  std::ifstream fin(filepath);
  std::stringstream buffer;

  StringVector res;
  if (fin.is_open()) {
    buffer << fin.rdbuf();
  } else {
    stop("Cannot open file: %s\n", filepath.c_str());
  }
  std::string line;
  while (getline(buffer, line)) {
    if (!line.empty() && line[0] != '*') {
      res.push_back(line);
    }
  }
  return res;
}
