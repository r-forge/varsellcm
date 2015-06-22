DATA notes;
INPUT nom $ math scie fran lati dm;
CARDS;
jean 6 6.0 5 5.5 8.0
aline 8.0 8.0 8.0 8.0 9.0
annie 6 7 11 9.5 11.0
monique 14.5 14.5 15.5 15 8.0
didier 14 14 12 12.5 10.0
andre 11 10 5.5 7 13.0
pierre 5.5 7 14 11.5 10.0
brigitte 13 12.5 8.5 9.5 12.0
evelyne 9 9.5 12.5 12 18.0
;
run;


proc print;
  title1 "Notes";
run;

/* -------------- */
/* acp non normee */
/* -------------- */

proc princomp data=notes
 cov /* option d'acp non normee */
  vardef = N /* variance empirique donc 1/N et non 1/(N-1) */
  out = C /* tableau des données initiales concaténé au tableau des coordonnees factorielles */
  outstat = mat_cov_vp; /* fichier contenant 1 colonne pour chaque variable analysee, ainsi qu une 
                           ou plusieurs lignes par type de statistique (moyennes, nb d'observations, 
                           mat. des covariances, val pp et vec pp (le type est indique par la variable _TYPE_) */
                          
run;

