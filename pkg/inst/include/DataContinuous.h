#ifndef DataContinuous_H
#define DataContinuous_H

#include "Data.h"

class DataContinuous : public Data{
  public:
    Mat<double> m_x, m_priors, m_notNA;
    int m_nrows, m_ncols;
  
  DataContinuous(){};
  DataContinuous(const S4 &);
  ~DataContinuous(){};
  
};
#endif