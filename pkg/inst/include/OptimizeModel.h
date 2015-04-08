#ifndef Tools_H
#define Tools_H


#include "DataContinuous.h"
#include "DataCategorical.h"

/*
Algorithm OptimizeModel(DataContinuous & data, int g, int nbinit, int searchmodel){
  Algorithm bestmodel(ones<vec>(data.m_ncols), g, data);  
  if (nbinit >0){
    Mat<double> tmp=randu<mat>(data.m_ncols, nbinit);
    Mat<double> omegainit = ones<mat>(data.m_ncols, nbinit);
    if (searchmodel == 1){
      for (int ini=0; ini<nbinit; ini++){
        uvec loc = find(tmp.col(ini)>0.5);
        Col<double> tmpvec = zeros<vec>(data.m_ncols);
        tmpvec(loc)+=1;
        omegainit.col(ini)=tmpvec;
      }    
      bestmodel=Algorithm(omegainit.col(0), g, data);
      bestmodel.Run(data);
      if (omegainit.n_cols > 1){
        Algorithm candmodel(bestmodel);
        for (int it=1;it<omegainit.n_cols;it++){
          candmodel.Run(omegainit.col(it), data);
          if (candmodel.m_micl>bestmodel.m_micl){
            bestmodel.actu(candmodel);
          }
        }
      }    
  }  
  return bestmodel;
}


Algorithm OptimizeModel(const DataCategorical & data, int g, int nbinit, int searchmodel){
  Algorithm bestmodel(ones<vec>(data.m_ncols), g, data);  
  if (nbinit >0){
    Mat<double> tmp=randu<mat>(data.m_ncols, nbinit);
    Mat<double> omegainit = ones<mat>(data.m_ncols, nbinit);
      for (int ini=0; ini<nbinit; ini++){
        uvec loc = find(tmp.col(ini)>0.5);
        Col<double> tmpvec = zeros<vec>(data.m_ncols);
        tmpvec(loc)+=1;
        omegainit.col(ini)=tmpvec;
      }    
      bestmodel=Algorithm(omegainit.col(0), g, data);
      bestmodel.Run(data);
      if (omegainit.n_cols > 1){
        Algorithm candmodel(bestmodel);
        for (int it=1;it<omegainit.n_cols;it++){
          candmodel.Run(omegainit.col(it), data);
          if (candmodel.m_micl>bestmodel.m_micl){
            bestmodel.actu(candmodel);
          }
        }
      }    
    
  }
  
  return bestmodel;
}






*/
#endif