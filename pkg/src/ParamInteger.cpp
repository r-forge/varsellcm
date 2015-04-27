#include "ParamInteger.h"

ParamInteger::ParamInteger(){
  this->m_lambda = ones<mat>(0,0);
  this->m_pi = ones<vec>(0);  
}

ParamInteger::ParamInteger(const ParamInteger & param){
  this->m_lambda = param.m_lambda;
  this->m_pi = param.m_pi;  
}

ParamInteger::ParamInteger(const DataInteger * data,  const colvec & omega, const int & g){
  this->m_lambda = ones<mat>(g, sum(omega));
  this->m_pi = ones<vec>(g)/g;  
  int k=0, li=0;
  if (sum(omega)>0){
    uvec location = find(omega == 1);
    for (int j=0; j<m_lambda.n_cols; j++){
      ivec who = randi<ivec>(data->m_nrows, distr_param(0, data->m_nrows -1));
      k=0;
      li=0;
      while (k<g){
        if (data->m_notNA(who(li),location(j)) == 1){
          m_lambda(k,j) = data->m_x(who(li),location(j));
          k++;
        }
        li++;
      }
    }
  }
}