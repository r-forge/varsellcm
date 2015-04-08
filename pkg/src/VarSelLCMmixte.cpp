#include "AlgorithmContinuous.h"
#include "AlgorithmCategorical.h"
#include "XEMContinuous.h"
#include "XEMCategorical.h"

//[[Rcpp::export]]
S4  OptimizeMICL(S4 reference, StringVector name){
  S4 * reference_p=&reference;
  string namestr = as<std::string>(name);
  
  if (namestr == "Continuous"){
    DataContinuous * data_p = new DataContinuous(as<S4>(reference.slot("data")));
    AlgorithmContinuous *algo_p = new AlgorithmContinuous(data_p, reference_p);
    algo_p->Run(reference_p);
    XEMContinuous *xem_p  = new XEMContinuous(data_p, reference_p);
    xem_p->Run(); 
    xem_p->Output(reference_p);
  }else if (namestr == "Categorical"){
    DataCategorical * data_p = new DataCategorical(as<S4>(reference.slot("data")));
    AlgorithmCategorical *algo_p = new AlgorithmCategorical(data_p, reference_p);
    algo_p->Run(reference_p);
    XEMCategorical *xem_p  = new XEMCategorical(data_p, reference_p);
    xem_p->Run(); 
    xem_p->Output(reference_p);
  }
  
  return reference;
}