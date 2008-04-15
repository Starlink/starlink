/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmpv2el(double qfoo[],double qbar,double qbaz,int Q0,
int*qfobar,double*q1,double*q2,double*qfoobar,double*Q3,
double*q4,double*qfOBAz,double*qfoobaz,double*QQUUX,int*Q5)
#define QFRED 86400.0
#define qdog 0.01720209895
#define qcat 0.3977771559319137
#define QFISH 0.9174820620691818
#define QgASp 1e-3
#define Q6 1e-8
#define q7 1e-8
{double q8,QBAD,qBuG,qsilly,QBUGGY,QMUM,qDAd,q9,Q10,Q11,q12,
Q13,Q14,qdisk,Q15,q16,q17,QEMPTY,q18,QFULL,qfast,qsmall,QBIG
,QOK,QHELLO,QBYE,QMAGIC,q19,q20,qobSCUrE,QSPEED,qIndex,Q21,
qbill,q22,q23,qjoe,qemacs,q24,QVI,qrms,QfbI,Qcia;int Q25;if(
qbaz<0.0){*Q5=-1;return;}if(Q0<1||Q0>3){*Q5=-2;return;}Q25=
Q0;q8=qfoo[0];QBAD=qfoo[1]*QFISH+qfoo[2]*qcat;qBuG=-qfoo[1]*
qcat+qfoo[2]*QFISH;qsilly=QFRED*qfoo[3];QBUGGY=QFRED*(qfoo[4
]*QFISH+qfoo[5]*qcat);QMUM=QFRED*(-qfoo[4]*qcat+qfoo[5]*
QFISH);qDAd=sqrt(q8*q8+QBAD*QBAD+qBuG*qBuG);q9=qsilly*qsilly
+QBUGGY*QBUGGY+QMUM*QMUM;Q10=sqrt(q9);if(qDAd<QgASp||Q10<Q6)
{*Q5=-3;return;}Q11=q8*qsilly+QBAD*QBUGGY+qBuG*QMUM;q12=(1.0
+qbaz)*qdog*qdog;Q13=QBAD*QMUM-qBuG*QBUGGY;Q14=qBuG*qsilly-
q8*QMUM;qdisk=q8*QBUGGY-QBAD*qsilly;Q15=Q13*Q13+Q14*Q14;q16=
Q15+qdisk*qdisk;q17=sqrt(q16);QEMPTY=atan2(sqrt(Q15),qdisk);
q18=(Q13!=0.0||Q14!=0.0)?atan2(Q13,-Q14):0.0;QFULL=2.0/qDAd-
q9/q12;qfast=1.0-QFULL*q16/q12;qsmall=(qfast>=0.0)?sqrt(
qfast):0.0;QBIG=q17*Q11;QOK=q16-qDAd*q12;QHELLO=(QBIG!=0.0||
QOK!=0.0)?atan2(QBIG,QOK):0.0;QBIG=sin(q18);QOK=cos(q18);
QBYE=atan2((-q8*QBIG+QBAD*QOK)*cos(QEMPTY)+qBuG*sin(QEMPTY),
q8*QOK+QBAD*QBIG);QMAGIC=QBYE-QHELLO;if(fabs(qsmall-1.0)<q7)
qsmall=1.0;if(qsmall>=1.0)Q25=3;q19=q12*QFULL*QFULL*QFULL;
q20=qsmall-1.0;qobSCUrE=qsmall+1.0;QSPEED=QHELLO/2.0;qIndex=
sin(QSPEED);Q21=cos(QSPEED);q22=q23=qjoe=qemacs=q24=QVI=0.0;
if(qsmall<1.0){qbill=2.0*atan2(sqrt(-q20)*qIndex,sqrt(
qobSCUrE)*Q21);q22=qbill-qsmall*sin(qbill);q23=sqrt(q19);}if
(Q25==1){qjoe=q18+QMAGIC;qemacs=qjoe+q22;}if(Q25==3){q24=q16
/(q12*qobSCUrE);if(qsmall<1.0){QVI=qbar-q22/q23;}else{qrms=
qIndex/Q21;if(qsmall==1.0){QVI=qbar-qrms*(1.0+qrms*qrms/3.0)
*q17*q16/(2.0*q12*q12);}else{QfbI=sqrt(q20/qobSCUrE)*qrms;
Qcia=log(1.0+QfbI)-log(1.0-QfbI);QVI=qbar-(qsmall*sinh(Qcia)
-Qcia)/sqrt(-q19);}}}*qfobar=Q25;*q2=QEMPTY;*qfoobar=
sbmdranrm(q18);*qfOBAz=qsmall;if(Q25==1){*Q3=sbmdranrm(qjoe)
;*qfoobaz=sbmdranrm(qemacs);*QQUUX=q23;}else{*Q3=sbmdranrm(
QMAGIC);if(Q25==2)*qfoobaz=sbmdranrm(q22);}if(Q25!=3){*q1=
qbar;*q4=1.0/QFULL;}else{*q1=QVI;*q4=q24;}*Q5=0;}
