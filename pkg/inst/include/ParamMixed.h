/*
  Cette classes définie les paramètres pour des données qualitatives

Ces éléments sont:
  m_alpha : liste où m_alpha[j] donne les probabilités associées à chaque modalité pour chaque classe
m_pi : proportions

*/
#ifndef ParamMixed_H
#define ParamMixed_H

#include "DataMixed.h"
#include "ParamContinuous.h"
#include "ParamCategorical.h"

class ParamMixed : public Param{
  public:
  ParamContinuous m_paramContinuous;
  ParamCategorical m_paramCategorical;
  
  ParamMixed();
  ~ParamMixed(){};
    
  ParamMixed(const ParamMixed & param);
  ParamMixed(const DataMixed *, const colvec & , const int &);

};
#endif