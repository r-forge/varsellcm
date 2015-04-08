#ifndef Algorithm_H
#define Algorithm_H

#include <iostream>
#include <iomanip>
#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace std;
using namespace arma;
using namespace Rcpp;


class Algorithm{ 
  
  public:
  colvec m_zStarBest, m_zStarCurrent, m_zCandCurrent, m_integralenondiscrim;
  Mat<double> omegainit;
  double m_miclCurrent, m_miclBest;
  int m_g;
  Col<double> m_omegaCurrent, m_omegaBest;
  
  Algorithm(){};
  ~Algorithm(){};
  void InitCommumParamXEM(const int &, const int &, const int &, const int &) ;
  void Run();
  void Optimize_partition();
  
  virtual double Integre_Complete_Like_Cand() = 0;
  virtual void Optimize_model() = 0;
  virtual void zCandInit() = 0;
};
#endif  