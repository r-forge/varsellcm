/*
Cette classe contient les éléments relatifs aux données continues

Ces élements sont:
m_x : matrice des données
m_priors : matrice des hyper-paramètres (1 ligne par variable)
m_notNA : matrice binaire indiquant si l'observation est manquante ou non (attention dans m_x les valeurs
manquantes ont été artificellement remplacées par la valeur 0 pour pouvoir utiliser armadillo)
m_nrows : nombre d'individus
m_ncols : nombre de variables
*/
#ifndef DataContinuous_H
#define DataContinuous_H

#include <iostream>
#include <iomanip>
#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace std;
using namespace arma;
using namespace Rcpp;

class DataContinuous{
  public:
    Mat<double> m_x, m_priors, m_notNA;
    int m_nrows, m_ncols;
  
  DataContinuous(){};
  DataContinuous(const S4 &);
  ~DataContinuous(){};
  
};
#endif