
LIBNAME COMPIL 'Ici mettre le chemin de la macro';
OPTIONS SASMSTORE=COMPIL MSTORED NODATE PAGESIZE=66;


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


PROC PRINT;
TITLE 'Notes (de 0 à 20) données aux neuf étudiants';
RUN;


TITLE 'Analyse en composantes principales sur les notes';
%ACP(DATAACT=notes,
VARACT=math--dm,
ID=nom,
VECP=3,
IOA=3,
IVA=3,
IVS=3,
OUT=SOR,
NAXER=3,
FILL=ALL);


PROC PRINT DATA=SOR;
TITLE2 "Table en sortie créée par la macro";
RUN;


OPTIONS PAGESIZE=95;
TITLE2 "Les variables dans le plan 1-2";
%PLOTACP(AXEH=1,AXEV=2,POINTS=VARACT VARSUP);

OPTIONS PAGESIZE=66;
TITLE2 "Les individus dans le plan 1-2";

%PLOTACP(AXEH=1,AXEV=2,POINTS=OBSACT);
TITLE2 "Les individus et les axes unitaires dans le plan 1-2";
%PLOTACP(AXEH=1,AXEV=2,POINTS=OBSACT AXEUNI);

