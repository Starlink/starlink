/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmo2iqk(char*qfoo,double qbar,double qbaz,IOpars*Q0,
double*qfobar,double*q1)
#define q2 0.242535625
{int qfoobar;double Q3,q4,sphi,cphi,qfOBAz,qfoobaz,QQUUX,Q5,
QFRED[3],qdog,qcat,QFISH,QgASp,Q6,q7,refa,refb,q8,QBAD,qBuG,
qsilly,QBUGGY,QMUM,qDAd,q9,Q10,diurab,Q11,q12,Q13,Q14,xpl,
ypl,qdisk,Q15;qfoobar=(int)qfoo[0];Q3=qbar;q4=qbaz;sphi=Q0->
sphi;cphi=Q0->cphi;if(qfoobar=='r'||qfoobar=='R'){qfoobar=
'R';}else if(qfoobar=='h'||qfoobar=='H'){qfoobar='H';}else{
qfoobar='A';}if(qfoobar=='A'){qfOBAz=sin(q4);qfoobaz=-cos(Q3
)*qfOBAz;QQUUX=sin(Q3)*qfOBAz;Q5=cos(q4);}else{if(qfoobar==
'R')Q3=Q0->eral-Q3;sbmdcs2c(-Q3,q4,QFRED);qdog=QFRED[0];qcat
=QFRED[1];QFISH=QFRED[2];qfoobaz=sphi*qdog-cphi*QFISH;QQUUX=
qcat;Q5=cphi*qdog+sphi*QFISH;}QgASp=(qfoobaz!=0.0||QQUUX!=
0.0)?atan2(QQUUX,qfoobaz):0.0;Q6=sqrt(qfoobaz*qfoobaz+QQUUX*
QQUUX);q7=atan2(Q6,Q5);if(Q5>=q2){refa=Q0->refa;refb=Q0->
refb;q8=Q6/Q5;QBAD=(refa+refb*q8*q8)*q8;}else{sbmrefro(q7,Q0
->hm,Q0->tk,Q0->p,Q0->rh,Q0->wl,Q0->phi,Q0->tlr,1e-8,&QBAD);
}qBuG=q7+QBAD;qfOBAz=sin(qBuG);qsilly=cos(QgASp)*qfOBAz;
QBUGGY=sin(QgASp)*qfOBAz;QMUM=cos(qBuG);qDAd=sphi*qsilly+
cphi*QMUM;q9=QBUGGY;Q10=-cphi*qsilly+sphi*QMUM;diurab=-Q0->
diurab;Q11=(1.0-diurab*q9);q12=Q11*qDAd;Q13=Q11*(q9+diurab);
Q14=Q11*Q10;xpl=Q0->xpl;ypl=Q0->ypl;qdisk=xpl*q12-ypl*Q13+
Q14;QFRED[0]=q12-xpl*qdisk;QFRED[1]=Q13+ypl*qdisk;QFRED[2]=
qdisk-(xpl*xpl+ypl*ypl)*Q14;sbmdcc2s(QFRED,&Q15,q1);*qfobar=
sbmdranrm(Q0->eral+Q15);}
