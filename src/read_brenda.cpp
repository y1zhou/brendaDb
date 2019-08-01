#include <Rcpp.h>
#include <fstream>
#include <sstream>
#include <regex>

using namespace Rcpp;

//' @title Read raw BRENDA text file.
//'
//' @description
//'   Read file into buffer, and load all non-empty lines. Comment lines
//'   (starting with *) are skipped. The text file should be downloaded from
//'   https://www.brenda-enzymes.org/download_brenda_without_registration.php
//'
//' @param filepath A string indicating the path to the text file.
//'
//' @return A vector<string> with each element being a line in the file.
// [[Rcpp::export]]
std::vector<std::string> ReadBrendaFile(const std::string &filepath) {
  std::ifstream fin(filepath.c_str());
  std::stringstream buffer;

  std::vector<std::string> res;
  if (fin.is_open()) {
    buffer << fin.rdbuf();
  } else {
    stop("Cannot open file: %s\nPerhaps try using the absolute path?",
         filepath.c_str());
  }

  std::string line, line_need_fix = "";
  while (getline(buffer, line)) {
    if (line_need_fix != "") {
      line = line_need_fix + line;
      line_need_fix = "";
    }
    if (!line.empty() && line[0] != '*') {
      if (line[line.size() - 1] == '\r') {
        // Some lines end with carriage returns, in this case strip it and
        // append the next line
        line.erase(line.size() - 1);
        line_need_fix = line;
      } else {
        res.push_back(line);
      }
    }
  }
  return res;
}


//' @title Convert vector of lines to matrix.
//'
//' @description
//' For each EC entry, split the annotations into three columns:
//' - ID: EC number, e.g. 1.1.1.1
//' - field: the content of the information, e.g. protein, localization
//' - description: everything else
//'
//' @param lines The output vector<string> from `read_brenda_file`.
//'
//' @return A vector<vector<string>> containing information about the EC
//' entries. In R this is a list of 3 lists.
// [[Rcpp::export]]
std::vector<std::vector<std::string>> SeparateEntries(const std::vector<std::string> &lines) {
  std::regex field_regex("^[A-Z05_]{3,}$");  // IC50 field
  std::vector<std::string> colID, colField, colDescription;
  std::string current_ID = lines[0].substr(3),  // ID\tx.x.x.x, remove ID\t
              current_field = lines[1],  // PROTEIN, PH_OPTIMUM, etc.
              ec_info = "";
  const size_t nline = lines.size() - 1;
  for (size_t i = 2; i < nline; ++i) {  // Not reading last line
    // Skip first two lines because they're already read
    if (lines[i] == "///") {
      // /// indicates the end of an EC-number specific part
      // Insert previous entry, update ID and first field, clear ec_info
      colID.emplace_back(current_ID);
      colField.emplace_back(current_field);
      colDescription.emplace_back(ec_info);

      current_ID = lines[++i].substr(3);
      current_field = lines[++i];
      ec_info = "";
    } else {
      if (std::regex_match(lines[i].begin(), lines[i].end(), field_regex)) {
        // Insert previous entry, update field and clear ec_info
        colID.emplace_back(current_ID);
        colField.emplace_back(current_field);
        colDescription.emplace_back(ec_info);
        current_field = lines[i];
        ec_info = "";
      } else {
        // append to the current field
        ec_info = ec_info + lines[i] + '\n';
      }
    }
  }
  // Insert last entry
  colID.emplace_back(current_ID);
  colField.emplace_back(current_field);
  colDescription.emplace_back(ec_info);

  // Create vector of 3 columns
  std::vector<std::vector<std::string>> res = {colID, colField, colDescription};
  return res;
}
