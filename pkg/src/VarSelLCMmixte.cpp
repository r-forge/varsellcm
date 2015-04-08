#include "AlgorithmContinuous.h"
#include "AlgorithmCategorical.h"
#include "XEMContinuous.h"
#include "XEMCategorical.h"

//[[Rcpp::export]]
S4  OptimizeMICL(S4 reference, StringVector name){
  S4 dataS4 = reference.slot("data");
  S4 modelS4 = reference.slot("model");
  S4 strategyS4 = reference.slot("strategy");
  S4 partitionsS4 = reference.slot("partitions");
  S4 criteriaS4 = reference.slot("criteria");
  S4 paramS4 = reference.slot("param");
  string namestr = as<std::string>(name);
  int vbleSelec = strategyS4.slot("vbleSelec");
  int paramestim = strategyS4.slot("paramEstim");
  
  
  if (namestr == "Continuous"){
    
    DataContinuous * data_p = new DataContinuous(dataS4);
    
    if (vbleSelec == 1){
      AlgorithmContinuous * algo_p = new AlgorithmContinuous(data_p, modelS4.slot("g"), strategyS4.slot("initModel"));
      algo_p->Run();
      modelS4.slot("omega") = wrap(trans(algo_p->m_omegaBest));
      partitionsS4.slot("zOPT") = wrap(trans(algo_p->m_zStarBest));
      criteriaS4.slot("MICL") = algo_p->m_miclBest;     
    }
    
    if (paramestim == 1){
      XEMContinuous xem(data_p, modelS4.slot("omega"), modelS4.slot("g"), strategyS4);
      xem.Run(); 
      xem.Output(paramS4);
    }
    
  }else if (namestr == "Categorical"){
    
    DataCategorical * data_p = new DataCategorical(dataS4);
    
    if (vbleSelec == 1){
      
      AlgorithmCategorical * algo_p = new AlgorithmCategorical(data_p, modelS4.slot("g"), strategyS4.slot("initModel"));
      algo_p->Run();
      modelS4.slot("omega") = wrap(trans(algo_p->m_omegaBest));
      partitionsS4.slot("zOPT") = wrap(trans(algo_p->m_zStarBest));
      criteriaS4.slot("MICL") = algo_p->m_miclBest;     
    }
    
    if (paramestim == 1){
      XEMCategorical xem(data_p, modelS4.slot("omega"), modelS4.slot("g"), strategyS4);
      xem.Run(); 
      xem.Output(paramS4);
    }
    
  }
  return reference;
}