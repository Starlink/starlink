/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
static void qfoo(double,double,double,double,double,double,
double,double,double,double,double,double,double*,double*,
double*);static void qbar(double,double,double,double,double
,double*,double*);void sbmrefro(double qbaz,double hm,double
 Q0,double qfobar,double rh,double wl,double phi,double tlr,
double q1,double*q2)
#define qfoobar 16384
{static double Q3=1.623156204;static double q4=8314.32;
static double qfOBAz=28.9644;static double qfoobaz=18.0152;
static double QQUUX=6378120.0;static double Q5=18.36;static
double QFRED=11000.0;static double qdog=80000.0;double qcat;
double QFISH;double QgASp;double Q6;double q7;double q8,QBAD
,qBuG,qsilly,QBUGGY,QMUM;double qDAd;double q9;double Q10;
double Q11;int q12;int Q13,Q14,qdisk,Q15,q16;double q17,
QEMPTY,q18,QFULL,qfast,qsmall,QBIG,QOK,QHELLO,QBYE,QMAGIC,
q19,q20,qobSCUrE,QSPEED,qIndex,Q21,qbill,q22,q23,qjoe,qemacs
,q24,QVI,qrms,QfbI,Qcia,Q25,Q26,QNASA,QERR,Q27,Q28,qgoogle,
q29,QYahoO,Q30,qtrick,q31,Q32,QHINT,Q33,Q34,QBLAcK,Q35,q36,
q37,q38,q39,qred,QgreEN;
#define QYELLOW(q40,QBLUE) ((QBLUE)/(q40+QBLUE));
q17=sbmdrange(qbaz);QEMPTY=fabs(q17);QEMPTY=gmin(QEMPTY,Q3);
q18=gmax(hm,-1000.0);q18=gmin(q18,qdog);QFISH=gmax(Q0,100.0)
;QFISH=gmin(QFISH,500.0);QFULL=gmax(qfobar,0.0);QFULL=gmin(
QFULL,10000.0);qfast=gmax(rh,0.0);qfast=gmin(qfast,1.0);
qsmall=gmax(wl,0.1);QgASp=fabs(tlr);QgASp=gmax(QgASp,0.001);
QgASp=gmin(QgASp,0.01);QSPEED=fabs(q1);QSPEED=gmax(QSPEED,
1e-12);QBIG=gmin(QSPEED,0.1)/2.0;q12=(qsmall<=100.0);QOK=
qsmall*qsmall;QHELLO=9.784*(1.0-0.0026*cos(2.0*phi)-2.8e-7*
q18);QBYE=(q12)?((287.6155+1.62887/QOK+0.01360/(QOK*QOK))*
273.15/1013.25)*1e-6:77.6890e-6;Q11=QHELLO*qfOBAz/q4;QMAGIC=
Q11/QgASp;Q6=QMAGIC-2.0;q7=Q5-2.0;q19=QFISH-273.15;q20=pow(
10.0,(0.7859+0.03477*q19)/(1.0+0.00412*q19))*(1.0+QFULL*(
4.5e-6+6e-10*q19*q19));qobSCUrE=(QFULL>0.0)?qfast*q20/(1.0-(
1.0-qfast)*q20/QFULL):0.0;QSPEED=qobSCUrE*(1.0-qfoobaz/
qfOBAz)*QMAGIC/(Q5-QMAGIC);q8=QBYE*(QFULL+QSPEED)/QFISH;QBAD
=(QBYE*QSPEED+(q12?11.2684e-6:6.3938e-6)*qobSCUrE)/QFISH;
qBuG=(QMAGIC-1.0)*QgASp*q8/QFISH;qsilly=(Q5-1.0)*QgASp*QBAD/
QFISH;QBUGGY=q12?0.0:375463e-6*qobSCUrE/QFISH;QMUM=QBUGGY*q7
*QgASp/(QFISH*QFISH);qcat=QQUUX+q18;qfoo(qcat,QFISH,QgASp,Q6
,q7,q8,QBAD,qBuG,qsilly,QBUGGY,QMUM,qcat,&qIndex,&Q21,&qbill
);q22=Q21*qcat*sin(QEMPTY);q23=QYELLOW(Q21,qbill);qDAd=QQUUX
+gmax(QFRED,q18);qfoo(qcat,QFISH,QgASp,Q6,q7,q8,QBAD,qBuG,
qsilly,QBUGGY,QMUM,qDAd,&q9,&Q10,&qjoe);qemacs=asin(q22/(
qDAd*Q10));q24=QYELLOW(Q10,qjoe);qbar(qDAd,q9,Q10,Q11,qDAd,&
QVI,&qrms);QfbI=asin(q22/(qDAd*QVI));Qcia=QYELLOW(QVI,qrms);
Q25=QQUUX+qdog;qbar(qDAd,q9,Q10,Q11,Q25,&Q26,&QNASA);QERR=
asin(q22/(Q25*Q26));Q27=QYELLOW(Q26,QNASA);QgreEN=0.0;for(
Q14=1;Q14<=2;Q14++){Q28=1.0;Q13=8;if(Q14==1){qgoogle=QEMPTY;
q29=qemacs-qgoogle;QYahoO=q23;Q30=q24;}else{qgoogle=QfbI;q29
=QERR-qgoogle;QYahoO=Qcia;Q30=Q27;}qtrick=0.0;q31=0.0;qdisk=
1;for(;;){Q32=q29/(double)Q13;QHINT=(Q14==1)?qcat:qDAd;for(
Q15=1;Q15<Q13;Q15+=qdisk){Q33=sin(qgoogle+Q32*(double)Q15);
if(Q33>1e-20){QSPEED=q22/Q33;Q34=QHINT;q16=0;do{if(Q14==1){
qfoo(qcat,QFISH,QgASp,Q6,q7,q8,QBAD,qBuG,qsilly,QBUGGY,QMUM,
Q34,&Q35,&q36,&q37);}else{qbar(qDAd,q9,Q10,Q11,Q34,&q36,&q37
);}QBLAcK=(Q34*q36-QSPEED)/(q36+q37);Q34-=QBLAcK;}while(fabs
(QBLAcK)>1.0&&q16++<=4);QHINT=Q34;}if(Q14==1){qfoo(qcat,
QFISH,QgASp,Q6,q7,q8,QBAD,qBuG,qsilly,QBUGGY,QMUM,QHINT,&q38
,&q36,&q37);}else{qbar(qDAd,q9,Q10,Q11,QHINT,&q36,&q37);}q39
=QYELLOW(q36,q37);if(qdisk==1&&Q15%2==0){q31+=q39;}else{
qtrick+=q39;}}qred=Q32*(QYahoO+4.0*qtrick+2.0*q31+Q30)/3.0;
if(Q14==1)QgreEN=qred;if(fabs(qred-Q28)<=QBIG||Q13>=qfoobar)
break;Q28=qred;Q13+=Q13;q31+=qtrick;qtrick=0.0;qdisk=2;}}*q2
=QgreEN+qred;if(q17<0.0)*q2=-(*q2);}static void qfoo(double
qcat,double QFISH,double QgASp,double Q6,double q7,double q8
,double QBAD,double qBuG,double qsilly,double QBUGGY,double
QMUM,double QHINT,double*q38,double*q36,double*q37){double
QSPEED,QMAGENTA,QCyaN,Q41;QSPEED=QFISH-QgASp*(QHINT-qcat);
QSPEED=gmin(QSPEED,320.0);QSPEED=gmax(QSPEED,100.0);QMAGENTA
=QSPEED/QFISH;QCyaN=pow(QMAGENTA,Q6);Q41=pow(QMAGENTA,q7);*
q38=QSPEED;*q36=1.0+(q8*QCyaN-(QBAD-QBUGGY/QSPEED)*Q41)*
QMAGENTA;*q37=QHINT*(-qBuG*QCyaN+(qsilly-QMUM/QMAGENTA)*Q41)
;}static void qbar(double qDAd,double q9,double Q10,double
Q11,double QHINT,double*q36,double*q37){double QWHITE,QSPEED
;QWHITE=Q11/q9;QSPEED=(Q10-1.0)*exp(-QWHITE*(QHINT-qDAd));*
q36=1.0+QSPEED;*q37=-QHINT*QWHITE*QSPEED;}
