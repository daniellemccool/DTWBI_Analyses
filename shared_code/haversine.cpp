#include <math.h>
#include <Rcpp.h>
using namespace Rcpp;

double to_rad(double deg){
  return(deg * 3.141593 / 180);
}

// [[Rcpp::export]]
double haversine_cpp(double lat1, double lon1,
                     double lat2, double lon2){
  if ((lat1 == lat2) & (lon1 == lon2)) {
    return(0);
  }

  int radius_km = 6367; // recommended radius from https://www.themathdoctors.org/distances-on-earth-2-the-haversine-formula/
  double d_phi = to_rad(lat2 - lat1);    // diff of lats in rads
  double d_lambda = to_rad(lon2 - lon1); // diff of lons in rads
  double phi1 = to_rad(lat1); // startlat in rads
  double phi2 = to_rad(lat2); // endlat in rads
  double a = pow(sin(d_phi / 2), 2) + cos(phi1) * cos(phi2) * pow(sin(d_lambda / 2), 2); //square of half the chord length
  double c = 2 * atan2(sqrt(a), sqrt(1-a)); //gcd in rads
  double dist_km = radius_km * c;

  return(dist_km);
}
