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
  
  // Constructeurs et destructeurs par défaut (non utilisé)
  XEMContinuous(){};
  ~XEMContinuous(){};

  // Constructeurs avec et sans les paramètres de réglages
  XEMContinuous(const DataContinuous *, const colvec &, const int &);
  XEMContinuous(const DataContinuous *, const colvec &, const int &,  const S4 &);
  void InitSpecificParamXEMContinuous(const DataContinuous * datapasse);
    
  virtual void OneEM();
  virtual void ComputeTmpLogProba();
  virtual double ComputeLogLike();
  virtual void SwitchParamCurrent(int);
  virtual S4 Output(S4 &);
};
#endif