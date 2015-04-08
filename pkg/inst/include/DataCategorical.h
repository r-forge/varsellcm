#ifndef DataCategorical_H
#define DataCategorical_H

#include "Data.h"

class DataCategorical : public Data{
  public:
    Mat<double> m_profiles;
    rowvec m_nmodalities;
    colvec m_w;
    int m_nprofiles, m_ncols;
    vector < vector < uvec > > m_whotakewhat;
  
  DataCategorical();
  DataCategorical(const S4 &);
  ~DataCategorical(){};
};
#endif