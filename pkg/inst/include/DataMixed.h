/*
  Cette classe contient les éléments relatifs aux données mixtes

Ces élements sont:
*/
#ifndef DataMixed_H
#define DataMixed_H
  
#include "DataContinuous.h"
#include "DataCategorical.h"


class DataMixed : public Data{
  public:
    DataContinuous m_continuousData;
    DataCategorical m_categoricalData;
    bool m_withContinuous, m_withCategorical;
  
  DataMixed();
  DataMixed(const S4 &);
  ~DataMixed(){};
};
#endif