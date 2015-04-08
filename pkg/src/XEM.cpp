#include "XEM.h"

void XEM::InitCommumParamXEM(const colvec & om, const int & gv){
  nbSmall = 10;
  iterSmall = 20;
  nbKeep = 1;
  iterKeep = 1;
  tolKeep = 0.001;
  loglikeBest = log(0);
  loglikeSmall = ones<vec>(nbSmall) * log(0);
  omega = om;
  g=gv;
  location = find(omega == 1);
  iterCurrent = iterSmall;
}


void XEM::InitCommumParamXEM(const colvec & om, const int & gv, const S4 & strategy){
  nbSmall = strategy.slot("nbSmall");
  iterSmall = strategy.slot("iterSmall");
  nbKeep = strategy.slot("nbKeep");
  iterKeep = strategy.slot("iterKeep");
  tolKeep = strategy.slot("tolKeep");
  loglikeBest = log(0);
  loglikeSmall = ones<vec>(nbSmall) * log(0);
  omega = om;
  g=gv;
  location = find(omega == 1);
  iterCurrent = iterSmall;
}


void XEM::Run(){
  // Partie Small EM
  for (int ini=0; ini<nbSmall; ini++){
    SwitchParamCurrent(ini);
    OneEM();
    loglikeSmall(ini) = ComputeLogLike();
  }
  // On conserve les meilleurs initialisations
  uvec indices = sort_index(loglikeSmall);
  iterCurrent = iterKeep;
  for (int tmp1=0; tmp1<nbKeep; tmp1++){
    SwitchParamCurrent(indices(nbSmall - tmp1 - 1));
    OneEM();
    loglikeSmall(indices(nbSmall - tmp1 - 1)) = ComputeLogLike();
  }
  indices = sort_index(loglikeSmall);
  int indicebest = indices(nbKeep-1);
  SwitchParamCurrent(indicebest);
}

colvec XEM::FindZMAP(){
  Col<double> zMAP=ones<vec>(tmplogproba.n_rows);
  uword  index;
  double max_val=0;
  for (int i=0; i<tmplogproba.n_rows; i++){
    max_val = (tmplogproba.row(i)).max(index);
    zMAP(i)=index;
  }
  return zMAP;
}

void XEM::Estep(){
  for (int k=0; k<g; k++) tmplogproba.col(k) = tmplogproba.col(k)/rowsums;    
}