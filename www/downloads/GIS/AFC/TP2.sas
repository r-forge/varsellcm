ods pdf file ="C:\Users\somerlinck\Desktop\sortie.pdf";
OPTIONS NOCENTER NOOVP;

data activites_et_logements;
  infile 'mettre l adresse du fichier'
    firstobs = 2
	DLM='09'x;
   input  activites $ hotel habitant propriete parents amis tente_caravane vilage_de_vacances divers;
run;

proc print;
  title1 "Logements de vacances";
run;

proc corresp data = activites_et_logements observed outc=res ;
var hotel habitant propriete parents amis tente_caravane vilage_de_vacances divers;
id activites;
run;

%plotit(data = res, datatype = corresp);
run;
ods pdf close;



DATA micronecton;
INPUT Station $ Meno Neme Stlo Eukr Thae Euhe Sear Sero Sesa Pasi Pamu Gee;
CARDS;
S1 204 80 4 0 0 0 0 0 0 1 1 0
S2 272 88 12 0 0 0 0 0 0 3 0 1
S3 528 18 7 0 0 0 0 0 0 4 0 0
S4 208 186 12 2 0 0 1 0 0 5 0 0
S5 74 30 3 3 0 0 0 0 0 2 2 0
S6 49 77 0 5 0 0 0 1 1 1 1 0
S7 39 35 4 2 2 0 0 0 0 4 0 2
S8 25 155 33 17 2 0 0 0 0 0 0 2
S9 5 127 30 21 0 0 0 0 0 0 1 0
S10 3 107 31 38 0 2 0 0 0 1 1 0
S12 2 10 1 118 0 0 1 0 0 0 0 0
;
run;

proc print;
  title1 "micronecton";
run;

proc corresp data =micronecton observed outc=res ;
var Meno Neme Stlo Eukr Thae Euhe Sear Sero Sesa Pasi Pamu Gee;
id Station;
run;

%plotit(data = res, datatype = corresp);
run;
ods pdf close;



