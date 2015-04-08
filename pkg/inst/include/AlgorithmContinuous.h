#ifndef AlgorithmContinuous_H
#define AlgorithmContinuous_H

#include "DataContinuous.h"
#include "Algorithm.h"


class AlgorithmContinuous : public Algorithm{ 
  
  public:
  const DataContinuous * data_p;
  
  AlgorithmContinuous(){};
  ~AlgorithmContinuous(){};
  
  AlgorithmContinuous(const DataContinuous *, const S4 *);
  void InitSpecificParamAlgo(const DataContinuous * datapasse);
  
  
  virtual double Integre_Complete_Like_Cand();
  virtual void Optimize_model();
  virtual void zCandInit();  
  
  double IntegreOneVariable(const vec & v, const int & j);
};
#endif