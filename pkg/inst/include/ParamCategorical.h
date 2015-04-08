#ifndef ParamCategorical_H
#define ParamCategorical_H

#include "DataCategorical.h"


class ParamCategorical{
  public:
  vector< Mat<double> > m_alpha;
  Col<double> m_pi;
  
  ParamCategorical();
  ParamCategorical(const ParamCategorical & param);
  ParamCategorical(const DataCategorical *, const colvec & , const int &);
  // Celui-ci sera a supprimer
  ParamCategorical(const DataCategorical &, const colvec & , const int &);
  ~ParamCategorical(){};
};
#endif