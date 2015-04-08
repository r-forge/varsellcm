#ifndef XEMContinuous_H
#define XEMContinuous_H
#include "DataContinuous.h"
#include "ParamContinuous.h"
#include "XEM.h"


class XEMContinuous : public XEM{

    
  public:
  vector<ParamContinuous> paramCand;
  ParamContinuous * paramCurrent_p;
  const DataContinuous * data_p;
  Col<double> m_weightTMP;
  
  // Constructeurs et destructeurs par défaut (non utilisé)
  XEMContinuous(){};
  ~XEMContinuous(){};

  // Constructeurs avec et sans les paramètres de réglages
  XEMContinuous(const DataContinuous *, const colvec &, const int &);
  XEMContinuous(const DataContinuous *,  const S4 *);
  void InitSpecificParamXEMContinuous(const DataContinuous * datapasse);
    
  virtual void Mstep();
  virtual void ComputeTmpLogProba();
  virtual double ComputeLogLike();
  virtual void SwitchParamCurrent(int);
  virtual void Output(S4 *);
};
#endif