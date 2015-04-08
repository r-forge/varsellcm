#ifndef XEM_H
#define XEM_H

#include "DataContinuous.h"

class XEM{
  public:
  int nbSmall, iterSmall, nbKeep, iterKeep, iterCurrent, g;
  double tolKeep;
  Col<double> loglikeSmall, omega, rowsums, maxtmplogproba;
  Mat<double> tmplogproba;
  uvec location;
  bool paramEstim;
  
  XEM(){};
  void InitCommumParamXEM(const colvec &, const int &);
  void InitCommumParamXEM(const colvec &, const int &, const S4 &);
  ~XEM(){};
  
  virtual void Mstep() = 0;
  virtual void ComputeTmpLogProba() = 0;
  virtual double ComputeLogLike() = 0;
  virtual void SwitchParamCurrent(int) = 0;
  virtual void Output(S4 *)=0;
  
  void Run();
  void Estep();
  void OneEM();
  colvec FindZMAP();
};
#endif