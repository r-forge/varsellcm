#ifndef XEMPEN_H
#define XEMPEN_H

#include "DataMixed.h"
#include "ParamMixed.h"

class XEMPen{
  public:
  DataMixed *data_p;
  S4 * strategy_p;
  Col<double> loglikepen, rowsums, maxtmplogproba, m_weightTMP, m_loglikenondis, munondisc, sdnondisc; 
  int nbSmall, iterSmall, nbKeep, iterKeep, iterCurrent, g, m_nbdegenere;
  double tolKeep, m_penalty;
  vector< Col<double> > omegaCand,  nbparamCand;
  vector <ParamMixed> paramCand;
  Mat<double> tmplogproba ;
  Col<double> * omegaCurrent_p;
  ParamMixed * paramCurrent_p;
  Col<double> *  nbparamCurrent_p;

  // Constructeurs et destructeurs par défaut (non utilisé)
  XEMPen(){};
  XEMPen(const S4 *, const double);
  ~XEMPen(){};
    
  // Les quatres fonctions suivantes sont communes pour les classes héritiaires d'XEM.  
  // Lance l'estimation
  void Run();
  // Etape E
  void Estep(){for (int k=0; k<g; k++) tmplogproba.col(k) = tmplogproba.col(k)/rowsums;};
  // Effectue un EM
  void OneEM();
  // Renvoie la partition MAP
  colvec FindZMAP();
  double ComputeLoglikepen();
  int FiltreDegenere();
  
  // Les trois fonction suivantes sont à redéfinir pour chaque classe héritiaire d'XEM
  // Etape M
  void Mstep();
  // calcul des log-proba conditionnelles
  void ComputeTmpLogProba();
  // change le pointeur des paramètres acutels (voir définition spécifique des classes)
  void SwitchCurrent(int);
  // Acutalise l'object S4 retourné sous R
  void Output(S4 *);
  
};
#endif