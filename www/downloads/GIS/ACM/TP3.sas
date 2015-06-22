ods pdf file ="H:\A2-A4_Analyse_de_donnees\TP3_ACM\Exo2\sortie.pdf";
OPTIONS NOCENTER NOOVP;

data chiens;
infile 'H:\A2-A4_Analyse_de_donnees\TP3_ACM\chiens.txt'
firstobs=2 /* enlève la 1ère ligne */
DLM="09"x;
informat race $20. taille $1. poids $1. veloc $1. intell $1. affect $1. agress $1. fonct $1.;
format race $20. taille $1. poids $1. veloc $1. intell $1. affect $1. agress $1. fonct $1.;
input race $ taille $ poids $ veloc $ intell $ affect $ agress $ fonct $;
if _n_ = 1 then delete;
run;
proc print;
run;

/* Transformation des donnees : (ex : 1->taille1 etc...)*/
data chiens;
set chiens;
taill = ("taille"!!taille);  /* Renommage des modalités */
poid = ("poids"!!poids);
veloce = ("velocite"!!veloc); 
intel = ("intell"!!intell); 
affectueu =("affect"!!affect);
agressif= ("agress"!!agress);
fonction=("fonct"!!fonct);
run;

proc print;
run;

/* ACM tableau de Burt */
title1 "ACM Burt";
proc corresp data=chiens mca /*option pour Burt*/ outc=resBurt dimens=5 observed;
tables taill  poid  veloce  intel  affectueu  agressif fonction;
supplementary fonction; /*variable suppl*/
run;

proc print;
run;

/* graphique */
%plotit(data=resBurt, datatype=corresp)

/* tableau disjonctif complet */
proc corresp data = chiens binary outc =resTDC dimens=5 observed; /*ACM  avec fonction en supplémentaire */ 
tables race,taill  poid  veloce  intel  affectueu  agressif fonction;
supplementary fonction;
run;

proc print;
run;

/* graphique */
%plotit(data=resTDC, datatype=corresp)

ods pdf close;
