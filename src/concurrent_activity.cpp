#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector concurrentActivity(int outlen, NumericVector StartSecond, NumericVector Duration) {
  NumericVector cca(outlen);        // Result will go in this vector
  int k, j, iduration, istart;
  int n = StartSecond.size();       // Number of events

  for(int i = 0; i < n; ++i) {      // Process each event
    istart = round(StartSecond[i]);
    iduration = round(Duration[i]); // Number of bins to increment
    if (iduration == 0) {           // Just increment one bin
      cca[istart]++;
    } else {
      k = istart + iduration;            // Last bin to increment
      if (k >= outlen) {k = outlen - 1;} // Don't go past end of vector
      for (j = istart; j <= k; j++) {    // Increment bins covering the event duration
	cca[j]++;
      }
    }
  }
  return cca;
}



