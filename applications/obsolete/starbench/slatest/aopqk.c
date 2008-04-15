/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmaopqk(double qfoo,double qbar,double qbaz[14],double
*Q0,double*qfobar,double*q1,double*q2,double*qfoobar){static
 double Q3=0.242535625;int q4;double sphi,cphi,qfOBAz,
qfoobaz[3],QQUUX,Q5,QFRED,diurab,qdog,qcat,QFISH,QgASp,Q6,q7
,q8,QBAD,qBuG,refa,refb,qsilly,QBUGGY,QMUM,qDAd,q9,Q10,Q11,
q12,Q13,Q14;sphi=qbaz[1];cphi=qbaz[2];qfOBAz=qbaz[13];
sbmdcs2c(qfoo-qfOBAz,qbar,qfoobaz);QQUUX=qfoobaz[0];Q5=
qfoobaz[1];QFRED=qfoobaz[2];diurab=qbaz[3];qdog=1.0-diurab*
Q5;qcat=qdog*QQUUX;QFISH=qdog*(Q5+diurab);QgASp=qdog*QFRED;
Q6=sphi*qcat-cphi*QgASp;q7=QFISH;q8=cphi*qcat+sphi*QgASp;
QBAD=(Q6!=0.0||q7!=0.0)?atan2(q7,-Q6):0.0;qBuG=atan2(sqrt(Q6
*Q6+q7*q7),q8);refa=qbaz[10];refb=qbaz[11];sbmrefz(qBuG,refa
,refb,&qsilly);if(cos(qsilly)<Q3){q4=1;do{sbmrefro(qsilly,
qbaz[4],qbaz[5],qbaz[6],qbaz[7],qbaz[8],qbaz[0],qbaz[9],1e-8
,&QMUM);QBUGGY=qsilly+QMUM-qBuG;qsilly-=QBUGGY;q4++;}while(
fabs(QBUGGY)>1e-10&&q4<=10);}qDAd=sin(qsilly);q9=-cos(QBAD)*
qDAd;Q10=sin(QBAD)*qDAd;Q11=cos(qsilly);qfoobaz[0]=sphi*q9+
cphi*Q11;qfoobaz[1]=Q10;qfoobaz[2]=-cphi*q9+sphi*Q11;
sbmdcc2s(qfoobaz,&q12,&Q13);Q14=sbmdranrm(qfOBAz+q12);*Q0=
QBAD;*qfobar=qsilly;*q1=-q12;*q2=Q13;*qfoobar=Q14;}
