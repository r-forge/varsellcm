#ifndef XEM_H
#define XEM_H

#include "DataContinuous.h"

class XEM{
  public:
  int nbSmall, iterSmall, nbKeep, iterKeep, iterCurrent, g;
  double tolKeep, loglikeBest;
  Col<double> loglikeSmall, omega, rowsums, maxtmplogproba;
  Mat<double> tmplogproba;
  uvec location;
  
  XEM(){};
  void InitCommumParamXEM(const colvec &, const int &);
  void InitCommumParamXEM(const colvec &, const int &, const S4 &);
  ~XEM(){};
  
  virtual void OneEM() = 0;
  virtual void ComputeTmpLogProba() = 0;
  virtual double ComputeLogLike() = 0;
  virtual void SwitchParamCurrent(int) = 0;
  virtual S4 Output(S4 &)=0;
  
  void Run();
  colvec FindZMAP();
  void Estep();
};
#endif