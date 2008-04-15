/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmfk524(double qfoo,double qbar,double qbaz,double Q0,
double qfobar,double q1,double*q2,double*qfoobar,double*Q3,
double*q4,double*qfOBAz,double*qfoobaz){double QQUUX,Q5,
QFRED,qdog,qcat,QFISH;double QgASp,Q6,q7,q8,QBAD,qBuG,qsilly
,QBUGGY;double QMUM[6],qDAd[6];double q9,Q10,Q11;double q12,
Q13,Q14,qdisk;int Q15,q16;static double q17=100.0*60.0*60.0*
360.0/D2PI;static double QEMPTY=1.0e-30;static double q18=
21.095;static double QFULL[6]={-1.62557e-6,-0.31919e-6,-
0.13843e-6,1.245e-3,-1.580e-3,-0.659e-3};static double qfast
[6][6]={{0.9999256795,0.0111814828,0.0048590039,-
0.00000242389840,-0.00000002710544,-0.00000001177742},{-
0.0111814828,0.9999374849,-0.0000271771,0.00000002710544,-
0.00000242392702,0.00000000006585},{-0.0048590040,-
0.0000271557,0.9999881946,0.00000001177742,0.00000000006585,
-0.00000242404995},{-0.000551,0.238509,-0.435614,0.99990432,
0.01118145,0.00485852},{-0.238560,-0.002667,0.012254,-
0.01118145,0.99991613,-0.00002717},{0.435730,-0.008541,
0.002117,-0.00485852,-0.00002716,0.99996684}};QQUUX=qfoo;Q5=
qbar;QFRED=qbaz*q17;qdog=Q0*q17;qcat=qfobar;QFISH=q1;QgASp=
sin(QQUUX);Q6=cos(QQUUX);q7=sin(Q5);q8=cos(Q5);QBAD=Q6*q8;
qBuG=QgASp*q8;qsilly=q7;QBUGGY=q18*QFISH*qcat;QMUM[0]=QBAD;
QMUM[1]=qBuG;QMUM[2]=qsilly;QMUM[3]=-QFRED*qBuG-Q6*q7*qdog+
QBUGGY*QBAD;QMUM[4]=QFRED*QBAD-QgASp*q7*qdog+QBUGGY*qBuG;
QMUM[5]=q8*qdog+QBUGGY*qsilly;for(Q15=0;Q15<6;Q15++){QBUGGY=
0.0;for(q16=0;q16<6;q16++){QBUGGY+=qfast[Q15][q16]*QMUM[q16]
;}qDAd[Q15]=QBUGGY;}QBAD=qDAd[0];qBuG=qDAd[1];qsilly=qDAd[2]
;q12=sqrt(QBAD*QBAD+qBuG*qBuG+qsilly*qsilly);QBUGGY=QBAD*
QFULL[0]+qBuG*QFULL[1]+qsilly*QFULL[2];QBAD+=QFULL[0]*q12-
QBUGGY*QBAD;qBuG+=QFULL[1]*q12-QBUGGY*qBuG;qsilly+=QFULL[2]*
q12-QBUGGY*qsilly;q12=sqrt(QBAD*QBAD+qBuG*qBuG+qsilly*qsilly
);QBAD=qDAd[0];qBuG=qDAd[1];qsilly=qDAd[2];QBUGGY=QBAD*QFULL
[0]+qBuG*QFULL[1]+qsilly*QFULL[2];Q13=QBAD*QFULL[3]+qBuG*
QFULL[4]+qsilly*QFULL[5];QBAD+=QFULL[0]*q12-QBUGGY*QBAD;qBuG
+=QFULL[1]*q12-QBUGGY*qBuG;qsilly+=QFULL[2]*q12-QBUGGY*
qsilly;q9=qDAd[3]+QFULL[3]*q12-Q13*QBAD;Q10=qDAd[4]+QFULL[4]
*q12-Q13*qBuG;Q11=qDAd[5]+QFULL[5]*q12-Q13*qsilly;Q14=QBAD*
QBAD+qBuG*qBuG;qdisk=sqrt(Q14);QQUUX=(QBAD!=0.0||qBuG!=0.0)?
atan2(qBuG,QBAD):0.0;if(QQUUX<0.0)QQUUX+=D2PI;Q5=atan2(
qsilly,qdisk);if(qdisk>QEMPTY){QFRED=(QBAD*Q10-qBuG*q9)/Q14;
qdog=(Q11*Q14-qsilly*(QBAD*q9+qBuG*Q10))/((Q14+qsilly*qsilly
)*qdisk);}if(qcat>QEMPTY){QFISH=(QBAD*q9+qBuG*Q10+qsilly*Q11
)/(qcat*q18*q12);qcat=qcat/q12;}*q2=QQUUX;*qfoobar=Q5;*Q3=
QFRED/q17;*q4=qdog/q17;*qfoobaz=QFISH;*qfOBAz=qcat;}
