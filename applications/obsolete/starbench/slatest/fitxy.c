/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmfitxy(int qfoo,int qbar,double qbaz[][2],double Q0[]
[2],double qfobar[6],int*q1){int q2,qfoobar;int Q3[4];int q4
;double qfOBAz,qfoobaz,QQUUX,Q5,QFRED,qdog,qcat,QFISH,QgASp,
p,Q6,q7,q8,QBAD,qBuG,qsilly,QBUGGY,QMUM,qDAd,q9,Q10,Q11,q12,
Q13,Q14,qdisk[4],Q15[3][3],q16[4][4],q17,QEMPTY,q18,QFULL,
qfast,qsmall,QBIG,QOK;*q1=0;qfOBAz=qfoobaz=QQUUX=Q5=QFRED=
qdog=qcat=QFISH=QgASp=0.0;p=(double)qbar;if(qfoo==6){if(qbar
>=3){Q6=0.0;q7=0.0;q8=0.0;QBAD=0.0;qBuG=0.0;qsilly=0.0;
QBUGGY=0.0;QMUM=0.0;qDAd=0.0;q9=0.0;Q10=0.0;for(q2=0;q2<qbar
;q2++){Q11=qbaz[q2][0];q12=qbaz[q2][1];Q13=Q0[q2][0];Q14=Q0[
q2][1];Q6=Q6+Q11;q7=q7+Q11*Q13;q8=q8+Q11*Q14;QBAD=QBAD+q12;
qBuG=qBuG+q12*Q14;qsilly=qsilly+q12*Q13;QBUGGY=QBUGGY+Q13;
QMUM=QMUM+Q14;qDAd=qDAd+Q13*Q13;q9=q9+Q13*Q14;Q10=Q10+Q14*
Q14;}qdisk[0]=Q6;qdisk[1]=q7;qdisk[2]=q8;Q15[0][0]=p;Q15[0][
1]=QBUGGY;Q15[0][2]=QMUM;Q15[1][0]=QBUGGY;Q15[1][1]=qDAd;Q15
[1][2]=q9;Q15[2][0]=QMUM;Q15[2][1]=q9;Q15[2][2]=Q10;sbmdmat(
3,Q15[0],qdisk,&q17,&qfoobar,Q3);if(qfoobar==0){for(q2=0;q2<
3;q2++){qfobar[q2]=qdisk[q2];}qdisk[0]=QBAD;qdisk[1]=qsilly;
qdisk[2]=qBuG;sbmdmxv(Q15,qdisk,&qfobar[3]);}else{*q1=-3;}}
else{*q1=-2;}}else if(qfoo==4){if(qbar>=2){for(q4=1;q4<=2;q4
++){QEMPTY=(q4==1)?1.0:-1.0;Q6=0.0;q18=0.0;QFULL=0.0;QBAD=
0.0;QBUGGY=0.0;QMUM=0.0;qfast=0.0;for(q2=0;q2<qbar;q2++){Q11
=qbaz[q2][0]*QEMPTY;q12=qbaz[q2][1];Q13=Q0[q2][0];Q14=Q0[q2]
[1];Q6=Q6+Q11;q18=q18+Q11*Q13+q12*Q14;QFULL=QFULL+Q11*Q14-
q12*Q13;QBAD=QBAD+q12;QBUGGY=QBUGGY+Q13;QMUM=QMUM+Q14;qfast=
qfast+Q13*Q13+Q14*Q14;}qdisk[0]=Q6;qdisk[1]=q18;qdisk[2]=
QFULL;qdisk[3]=QBAD;q16[0][0]=p;q16[0][1]=QBUGGY;q16[0][2]=-
QMUM;q16[0][3]=0.0;q16[1][0]=QBUGGY;q16[1][1]=qfast;q16[1][2
]=0.0;q16[1][3]=QMUM;q16[2][0]=QMUM;q16[2][1]=0.0;q16[2][2]=
-qfast;q16[2][3]=-QBUGGY;q16[3][0]=0.0;q16[3][1]=QMUM;q16[3]
[2]=QBUGGY;q16[3][3]=p;sbmdmat(4,q16[0],qdisk,&q17,&qfoobar,
Q3);if(qfoobar==0){qfOBAz=qdisk[0];qfoobaz=qdisk[1];QQUUX=
qdisk[2];Q5=qdisk[3];qsmall=0.0;for(q2=0;q2<qbar;q2++){Q13=
Q0[q2][0];Q14=Q0[q2][1];QBIG=qfOBAz+qfoobaz*Q13-QQUUX*Q14-
qbaz[q2][0]*QEMPTY;QOK=Q5+QQUUX*Q13+qfoobaz*Q14-qbaz[q2][1];
qsmall=qsmall+QBIG*QBIG+QOK*QOK;}if(q4==1){QFRED=qfOBAz;qdog
=qfoobaz;qcat=QQUUX;QFISH=Q5;QgASp=qsmall;}}else{qsmall=-1.0
;}}if(QgASp>=0.0&&QgASp<=qsmall){qfobar[0]=QFRED;qfobar[1]=
qdog;qfobar[2]=-qcat;qfobar[3]=QFISH;qfobar[4]=qcat;qfobar[5
]=qdog;}else if(qfoobar==0){qfobar[0]=-qfOBAz;qfobar[1]=-
qfoobaz;qfobar[2]=QQUUX;qfobar[3]=Q5;qfobar[4]=QQUUX;qfobar[
5]=qfoobaz;}else{*q1=-3;}}else{*q1=-2;}}else{*q1=-1;}}
