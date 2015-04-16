#include "XEMMixed.h"

Col<double> dlogGaussianbis(const Col<double> & x, const Col<double> & o, const double  mu, const double  sd){
  Col<double>tmpval= - 0.5*pow((x - mu),2)/pow(sd,2) - log(sd * sqrt( 2*M_PI));
  if (sum(o)<x.n_rows)  tmpval(find( o == 0)) = zeros<vec>(x.n_rows-sum(o));
  return  tmpval;
};

XEMMixed::XEMMixed(const DataMixed * datapasse, const S4 * reference_p){
  paramEstim = as<S4>(reference_p->slot("strategy")).slot("paramEstim");
  if (paramEstim){
    InitCommumParamXEM(as<S4>(reference_p->slot("model")).slot("omega"), as<S4>(reference_p->slot("model")).slot("g"), as<S4>(reference_p->slot("strategy")));
    InitSpecificParamXEMMixed(datapasse);
  }
}

XEMMixed::XEMMixed(const DataMixed * datapasse, const colvec & omega, const int & g){
  paramEstim = TRUE;
  InitCommumParamXEM(omega, g);  
  InitSpecificParamXEMMixed(datapasse);
}

void XEMMixed::InitSpecificParamXEMMixed(const DataMixed * datapasse){
  data_p = datapasse;
  int vu=0;
  if (data_p->m_withContinuous){
    omegaContinuous = omega.subvec(vu, vu + data_p->m_continuousData_p->m_ncols - 1);
    locationContinuous = find(omegaContinuous == 1);
    vu += data_p->m_continuousData_p->m_ncols;
  }
  if (data_p->m_withCategorical){
    omegaCategorical = omega.subvec(vu, vu + data_p->m_categoricalData_p->m_ncols - 1);
    locationCategorical = find(omegaCategorical == 1);
    vu += data_p->m_categoricalData_p->m_ncols;
  }
  for (int i=0;i<nbSmall;i++) paramCand.push_back( ParamMixed(data_p, omega, g) );
  tmplogproba = zeros<mat>(data_p->m_nrows, g);
  maxtmplogproba = ones<vec>(data_p->m_nrows);
  rowsums = ones<mat>(data_p->m_nrows);
  m_weightTMP=zeros<vec>(data_p->m_nrows);
}

void XEMMixed::SwitchParamCurrent(int ini){paramCurrent_p = &paramCand[ini];}


void XEMMixed::ComputeTmpLogProba(){
  for (int k=0; k<g; k++){
    m_weightTMP = zeros<vec>(data_p->m_nrows) + log(paramCurrent_p->m_pi(k));
    // partie continue  
    for (int j=0; j< sum(omegaContinuous); j++)
    m_weightTMP += dlogGaussianbis(data_p->m_continuousData_p->m_x.col(locationContinuous(j)), data_p->m_continuousData_p->m_notNA.col(locationContinuous(j)), paramCurrent_p->m_paramContinuous.m_mu(k,j),  paramCurrent_p->m_paramContinuous.m_sd(k,j));
    // partie qualitative    
    for (int j=0; j<sum(omegaCategorical); j++){
      for (int h=0; h<data_p->m_categoricalData_p->m_nmodalities(locationCategorical(j)); h++){
        m_weightTMP(data_p->m_categoricalData_p->m_whotakewhat[locationCategorical(j)][h]) += log(paramCurrent_p->m_paramCategorical.m_alpha[j](k,h));
      }
    }
    tmplogproba.col(k) = m_weightTMP;
  }  
}



void XEMMixed::Mstep(){
  paramCurrent_p->m_pi = trans(sum(tmplogproba,0));
  paramCurrent_p->m_pi = paramCurrent_p->m_pi / sum(paramCurrent_p->m_pi);
  
  // partie continue
  for (int k=0; k<g; k++){
    for (int j=0; j< sum(omegaContinuous); j++){
      m_weightTMP = tmplogproba.col(k) % data_p->m_continuousData_p->m_notNA.col(locationContinuous(j));
      paramCurrent_p->m_paramContinuous.m_mu(k,j) = sum(data_p->m_continuousData_p->m_x.col(locationContinuous(j)) % m_weightTMP ) / sum(m_weightTMP);
      paramCurrent_p->m_paramContinuous.m_sd(k,j) = sqrt(sum( pow(data_p->m_continuousData_p->m_x.col(locationContinuous(j)) - paramCurrent_p->m_paramContinuous.m_mu(k,j),2) % m_weightTMP) / sum(m_weightTMP));
    }
  }
  
  // partie qualitative
  for (int j=0; j< sum(omegaCategorical); j++){
    for (int h=0; h< data_p->m_categoricalData_p->m_nmodalities(locationCategorical(j)); h++){
      paramCurrent_p->m_paramCategorical.m_alpha[j].col(h) = trans(trans(data_p->m_categoricalData_p->m_w(data_p->m_categoricalData_p->m_whotakewhat[locationCategorical(j)][h])) *tmplogproba.rows(data_p->m_categoricalData_p->m_whotakewhat[locationCategorical(j)][h]));
    }
    for (int k=0; k<g; k++)
    paramCurrent_p->m_paramCategorical.m_alpha[j].row(k) = paramCurrent_p->m_paramCategorical.m_alpha[j].row(k)/sum(paramCurrent_p->m_paramCategorical.m_alpha[j].row(k));
  }
}

int XEMMixed::FiltreDegenere(){
  int output = 0;
  
  if (data_p->m_withContinuous){
    if (sum(omega.subvec(0,  data_p->m_continuousData_p->m_ncols - 1))>0){
        if (min(min(paramCurrent_p->m_paramContinuous.m_sd))<0.000001)
    output = 1;
    }
  }
  return output;

}

void XEMMixed::Output(S4 * reference_p){
  if (paramEstim){
    if (m_nbdegenere<nbKeep){
    // Partie Continue    
    Mat<double> mu=ones<mat>(g, data_p->m_continuousData_p->m_ncols);
    Mat<double> sd=ones<mat>(g, data_p->m_continuousData_p->m_ncols);
    int loc=0;
    
    for (int j=0; j<data_p->m_continuousData_p->m_ncols; j++){
      if (omegaContinuous(j) == 0){
        vec tmp = data_p->m_continuousData_p->m_x.col(j);
        vec keep = tmp(find(data_p->m_continuousData_p->m_notNA.col(j) == 1));
        mu.col(j) = mu.col(j)*mean(keep);
        sd.col(j) = sd.col(j)*sqrt(sum(pow(( keep - mean(keep)),2) ) / keep.n_rows);
        loglikeoutput += sum(dlogGaussianbis(data_p->m_continuousData_p->m_x.col(j), data_p->m_continuousData_p->m_notNA.col(j), mu(0,j), sd(0,j)));
      }else{
        mu.col(j) = paramCurrent_p->m_paramContinuous.m_mu.col(loc);
        sd.col(j) = paramCurrent_p->m_paramContinuous.m_sd.col(loc);
        loc ++;
      }
    }
    
    // Partie Categorielle
    vector< Mat<double> >  alpha;
    alpha.resize(data_p->m_categoricalData_p->m_ncols);
    loc=0;
    for (int j=0; j<data_p->m_categoricalData_p->m_ncols; j++){
      alpha[j] = zeros<mat>(g, data_p->m_categoricalData_p->m_nmodalities(j));
      if (omegaCategorical(j) == 0){
        vec tmp = zeros<vec>(data_p->m_categoricalData_p->m_nmodalities(j));
        for (int h=0; h<data_p->m_categoricalData_p->m_nmodalities(j); h++)
        tmp(h) = sum(data_p->m_categoricalData_p->m_w(data_p->m_categoricalData_p->m_whotakewhat[j][h]));
        tmp = tmp/sum(tmp);
        for (int k=0; k<g; k++) alpha[j].row(k)=trans(tmp);
        for (int h=0; h<data_p->m_categoricalData_p->m_nmodalities(j); h++){
          loglikeoutput += sum(data_p->m_categoricalData_p->m_w(data_p->m_categoricalData_p->m_whotakewhat[j][h])) * log(alpha[j](0,h));
        }
        
      }else{
        alpha[j]=paramCurrent_p->m_paramCategorical.m_alpha[loc];
        loc ++;
      }
    }
    as<S4>(reference_p->slot("param")).slot("pi") = wrap(trans(paramCurrent_p->m_pi));
    as<S4>(as<S4>(reference_p->slot("param")).slot("paramContinuous")).slot("pi") = wrap(trans(paramCurrent_p->m_pi));
    as<S4>(as<S4>(reference_p->slot("param")).slot("paramCategorical")).slot("pi")  = wrap(trans(paramCurrent_p->m_pi));
    as<S4>(as<S4>(reference_p->slot("param")).slot("paramCategorical")).slot("alpha") = wrap(alpha);    
    as<S4>(as<S4>(reference_p->slot("param")).slot("paramContinuous")).slot("mu") = wrap(trans(mu));
    as<S4>(as<S4>(reference_p->slot("param")).slot("paramContinuous")).slot("sd") = wrap(trans(sd));
    as<S4>(reference_p->slot("criteria")).slot("degeneracyrate") = m_nbdegenere/nbKeep;
    as<S4>(reference_p->slot("criteria")).slot("loglikelihood") = loglikeoutput;
    Estep();
    as<S4>(reference_p->slot("partitions")).slot("tik") = wrap(tmplogproba);
    as<S4>(reference_p->slot("partitions")).slot("zMAP") = wrap(FindZMAP());
    }else{
      as<S4>(reference_p->slot("criteria")).slot("degeneracyrate") = 1;
    }
  }
}
