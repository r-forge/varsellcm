#ifndef Data_H
#define Data_H

#include <iostream>
#include <iomanip>
#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace std;
using namespace arma;
using namespace Rcpp;

class Data{
  public:
  
  Data(){};
  ~Data(){};
  
  virtual void affiche(){};
};
#endif