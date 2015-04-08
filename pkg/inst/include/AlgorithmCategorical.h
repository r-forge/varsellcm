#ifndef AlgorithmCategorical_H
#define AlgorithmCategorical_H

#include "DataCategorical.h"
#include "Algorithm.h"


class AlgorithmCategorical : public Algorithm{ 
  
  public:
  const DataCategorical * data_p;
  
  AlgorithmCategorical(){};
  ~AlgorithmCategorical(){};
  
  AlgorithmCategorical(const DataCategorical *, const S4 *);
  void InitSpecificParamAlgo(const DataCategorical * datapasse);
  
  virtual double Integre_Complete_Like_Cand();
  virtual void Optimize_model();
  virtual void zCandInit();  
  
  double IntegreOneVariableCategoricalNotDiscrim(const int &);
  double IntegreOneVariableCategoricalDiscrim(const int &);
};
#endif