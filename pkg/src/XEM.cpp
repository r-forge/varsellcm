#include "XEM.h"

void XEM::InitCommumParamXEM(const colvec & om, const int & gv){
  nbSmall = 10;
  iterSmall = 20;
  nbKeep = 1;
  iterKeep = 1;
  tolKeep = 0.001;

/*  nbSmall = 250;
  iterSmall = 20;
  nbKeep = 50;
  iterKeep = 1000;
  tolKeep = 0.001;*/
  
  
  m_nbdegenere=0;
  loglikeSmall = ones<vec>(nbSmall) * log(0);
  omega = om;
  g=gv;
  location = find(omega == 1);
  iterCurrent = iterSmall;
  loglikeoutput=log(0);
}

void XEM::InitCommumParamXEM(const colvec & om, const int & gv, const S4 & strategy){
  nbSmall = strategy.slot("nbSmall");
  iterSmall = strategy.slot("iterSmall");
  nbKeep = strategy.slot("nbKeep");
  iterKeep = strategy.slot("iterKeep");
  tolKeep = strategy.slot("tolKeep");
  loglikeSmall = ones<vec>(nbSmall) * log(0);
  m_nbdegenere=0;
  omega = om;
  g=gv;
  location = find(omega == 1);
  iterCurrent = iterSmall;
  loglikeoutput=log(0);
}

void XEM::Run(){
  if (paramEstim){
    // Partie Small EM
    for (int ini=0; ini<nbSmall; ini++){
      SwitchParamCurrent(ini);
      OneEM();
      loglikeSmall(ini) = ComputeLogLike();
    }
    // On conserve les meilleurs initialisations
    uvec indices = sort_index(loglikeSmall);
    iterCurrent = iterKeep;
    m_nbdegenere = 0;
    for (int tmp1=0; tmp1<nbKeep; tmp1++){
      SwitchParamCurrent(indices(nbSmall - tmp1 - 1));
      OneEM();
      loglikeSmall(indices(nbSmall - tmp1 - 1)) = ComputeLogLike();
      m_nbdegenere += isnan(loglikeSmall(indices(nbSmall - tmp1 - 1)));
    }
    uword  index;
    double indicebest = (loglikeSmall).max(index);
    SwitchParamCurrent(index);
    loglikeoutput = ComputeLogLike();
    
  }
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

double XEM::ComputeLogLike(){
  ComputeTmpLogProba();
  maxtmplogproba = max(tmplogproba, 1);
  for (int k=0; k<g; k++) tmplogproba.col(k)-=maxtmplogproba;
  tmplogproba = exp(tmplogproba);
  rowsums = sum(tmplogproba,1);
  return sum(maxtmplogproba) + sum(log(rowsums));
}


void XEM::Estep(){for (int k=0; k<g; k++) tmplogproba.col(k) = tmplogproba.col(k)/rowsums;}

void XEM::OneEM(){
  double loglike = ComputeLogLike(), prec = log(0);
  int it=0;
  while ( (it<iterSmall) && ((loglike-prec)>tolKeep) ){
    it ++;
    Estep();
    Mstep();
    prec = loglike;
    loglike = ComputeLogLike();
  }
  // Pour la dégénérescence (pas la peine car on obtien nan)
  //if (loglike == - log(0)) loglike=log(0);
  // Une verif
  if (prec>(loglike+tolKeep)) cout << "pb EM " << endl;
}