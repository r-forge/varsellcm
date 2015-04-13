#include "ParamMixed.h"

ParamMixed::ParamMixed(){
  this->m_paramContinuous = ParamContinuous();
  this->m_paramCategorical = ParamCategorical();
  this->m_pi = ones<vec>(0);  
}

ParamMixed::ParamMixed(const ParamMixed & param){
  this->m_paramContinuous = param.m_paramContinuous;
  this->m_paramCategorical = param.m_paramCategorical;
  this->m_pi = param.m_pi;  
}

ParamMixed::ParamMixed(const DataMixed * data,  const colvec & omega, const int & g){
  this->m_pi = ones<vec>(g)/g;  
  int vu=0;
  if (data->m_withContinuous){
    m_paramContinuous = ParamContinuous(data->m_continuousData_p, omega.subvec(vu, vu + data->m_continuousData_p->m_ncols - 1), g);
    vu += data->m_continuousData_p->m_ncols;
  }
  if (data->m_withCategorical){
    m_paramCategorical = ParamCategorical(data->m_categoricalData_p, omega.subvec(vu, vu + data->m_categoricalData_p->m_ncols - 1), g);
    vu += data->m_categoricalData_p->m_ncols;
  }
}