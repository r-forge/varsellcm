#ifndef XEMCategorical_H
#define XEMCategorical_H


#include "DataCategorical.h"
#include "ParamCategorical.h"
#include "XEM.h"

class XEMCategorical : public XEM{
  public:
  vector<ParamCategorical> paramCand;
  ParamCategorical * paramCurrent_p;
  const DataCategorical * data_p;
   Col<double>tmpval;
  
  // Constructeurs et destructeurs par défaut (non utilisé)
  XEMCategorical(){};
  ~XEMCategorical(){};

  // Constructeurs avec et sans les paramètres de réglages
  XEMCategorical(const DataCategorical *, const colvec &, const int &);
  XEMCategorical(const DataCategorical *,  const S4 *);
  void InitSpecificParamXEMCategorical(const DataCategorical * datapasse);
  
  virtual void Mstep();
  virtual void ComputeTmpLogProba();
  virtual double ComputeLogLike();
  virtual void SwitchParamCurrent(int);
  virtual void Output(S4 *);
};
#endif