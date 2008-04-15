/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmel2ue(double qfoo,int qbar,double qbaz,double Q0,
double qfobar,double q1,double q2,double qfoobar,double Q3,
double q4,double qfOBAz[],int*qfoobaz)
#define QQUUX 0.01720209895
#define Q5 0.3977771559319137
#define QFRED 0.9174820620691818
{int qdog;double qcat,QFISH,QgASp,Q6,q7,q8,QBAD,qBuG,qsilly,
QBUGGY,QMUM,qDAd,q9,Q10,Q11,q12,Q13,Q14,qdisk,Q15,q16,q17,
QEMPTY,q18,QFULL,qfast,qsmall[13],QBIG[6];if(qbar<1||qbar>3)
{*qfoobaz=-1;return;}if(qfoobar<0.0||qfoobar>10.0||(qfoobar
>=1.0&&qbar!=3)){*qfoobaz=-2;return;}if(q2<=0.0){*qfoobaz=-3
;return;}if(qbar==1&&q4<=0.0){*qfoobaz=-4;return;}switch(
qbar){case 1:qcat=qbaz-(Q3-q1)/q4;QFISH=q1-qfobar;QgASp=q2*(
1.0-qfoobar);Q6=q4/QQUUX;q7=Q6*Q6*q2*q2*q2;break;case 2:qcat
=qbaz-Q3*sqrt(q2*q2*q2)/QQUUX;QFISH=q1;QgASp=q2*(1.0-qfoobar
);q7=1.0;break;default:qcat=qbaz;QFISH=q1;QgASp=q2;q7=1.0;}
q8=q7*(qfoobar-1.0)/QgASp;QBAD=sqrt(q8+2.0*q7/QgASp);qBuG=
sin(QFISH);qsilly=cos(QFISH);QBUGGY=sin(Q0);QMUM=cos(Q0);
qDAd=sin(qfobar);q9=cos(qfobar);Q10=QgASp*qsilly;Q11=QgASp*
qBuG;q12=Q11*QBUGGY;Q11=Q11*QMUM;Q13=Q10*q9-Q11*qDAd;Q11=Q10
*qDAd+Q11*q9;Q14=Q11*QFRED-q12*Q5;qdisk=Q11*Q5+q12*QFRED;Q10
=-QBAD*qBuG;Q11=QBAD*qsilly;q12=Q11*QBUGGY;Q11=Q11*QMUM;Q15=
Q10*q9-Q11*qDAd;Q11=Q10*qDAd+Q11*q9;q16=Q11*QFRED-q12*Q5;q17
=Q11*Q5+q12*QFRED;QEMPTY=(qfoo-qcat)*QQUUX;q18=QEMPTY/QgASp;
Q6=pow(3.0*QEMPTY+sqrt(9.0*QEMPTY*QEMPTY+8.0*QgASp*QgASp*
QgASp),1.0/3.0);QFULL=Q6-2.0*QgASp/Q6;qfast=(1.0-qfoobar)*
q18+qfoobar*QFULL;qsmall[0]=q7;qsmall[1]=q8;qsmall[2]=qcat;
qsmall[3]=Q13;qsmall[4]=Q14;qsmall[5]=qdisk;qsmall[6]=Q15;
qsmall[7]=q16;qsmall[8]=q17;qsmall[9]=QgASp;qsmall[10]=0.0;
qsmall[11]=qfoo;qsmall[12]=qfast;sbmue2pv(qfoo,qsmall,QBIG,&
qdog);if(qdog){*qfoobaz=-5;return;}sbmpv2ue(QBIG,qfoo,q7-1.0
,qfOBAz,&qdog);if(qdog){*qfoobaz=-5;return;}*qfoobaz=0;}
