/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
double qfoo(double qbar,double qbaz);void sbmsvd(int Q0,int
qfobar,int q1,int q2,double*qbar,double*qfoobar,double*Q3,
double*q4,int*qfOBAz)
#define qfoobaz 30
{int QQUUX,Q5,QFRED,qdog,qcat,QFISH,QgASp,Q6,q7;double q8,
QBAD,qBuG,qsilly,QBUGGY,QMUM,qDAd,q9,Q10,Q11,q12;QQUUX=Q5=0;
if(Q0<qfobar||Q0>q1||qfobar>q2){*qfOBAz=-1;}else{*qfOBAz=0;
q8=0.0;QBAD=0.0;qBuG=0.0;for(QFRED=0;QFRED<qfobar;QFRED++){
QQUUX=QFRED+1;q4[QFRED]=QBAD*q8;q8=0.0;qsilly=0.0;QBAD=0.0;
if(QFRED<Q0){for(qdog=QFRED;qdog<Q0;qdog++){QBAD+=fabs(qbar[
qdog*q2+QFRED]);}if(QBAD!=0.0){for(qdog=QFRED;qdog<Q0;qdog++
){QBUGGY=qbar[qdog*q2+QFRED]/QBAD;qbar[qdog*q2+QFRED]=QBUGGY
;qsilly+=QBUGGY*QBUGGY;}QMUM=qbar[QFRED*q2+QFRED];q8=-dsign(
sqrt(qsilly),QMUM);qDAd=QMUM*q8-qsilly;qbar[QFRED*q2+QFRED]=
QMUM-q8;if(QFRED!=qfobar-1){for(qcat=QQUUX;qcat<qfobar;qcat
++){qsilly=0.0;for(qdog=QFRED;qdog<Q0;qdog++){qsilly+=qbar[
qdog*q2+QFRED]*qbar[qdog*q2+qcat];}QMUM=qsilly/qDAd;for(qdog
=QFRED;qdog<Q0;qdog++){qbar[qdog*q2+qcat]+=QMUM*qbar[qdog*q2
+QFRED];}}}for(qdog=QFRED;qdog<Q0;qdog++){qbar[qdog*q2+QFRED
]*=QBAD;}}}qfoobar[QFRED]=QBAD*q8;q8=0.0;qsilly=0.0;QBAD=0.0
;if(QFRED<Q0&&QFRED!=qfobar-1){for(qdog=QQUUX;qdog<qfobar;
qdog++){QBAD+=fabs(qbar[QFRED*q2+qdog]);}if(QBAD!=0.0){for(
qdog=QQUUX;qdog<qfobar;qdog++){QBUGGY=qbar[QFRED*q2+qdog]/
QBAD;qbar[QFRED*q2+qdog]=QBUGGY;qsilly+=QBUGGY*QBUGGY;}QMUM=
qbar[QFRED*q2+QQUUX];q8=-dsign(sqrt(qsilly),QMUM);qDAd=QMUM*
q8-qsilly;qbar[QFRED*q2+QQUUX]=QMUM-q8;for(qdog=QQUUX;qdog<
qfobar;qdog++){q4[qdog]=qbar[QFRED*q2+qdog]/qDAd;}if(QFRED!=
Q0-1){for(qcat=QQUUX;qcat<Q0;qcat++){qsilly=0.0;for(qdog=
QQUUX;qdog<qfobar;qdog++){qsilly+=qbar[qcat*q2+qdog]*qbar[
QFRED*q2+qdog];}for(qdog=QQUUX;qdog<qfobar;qdog++){qbar[qcat
*q2+qdog]+=qsilly*q4[qdog];}}}for(qdog=QQUUX;qdog<qfobar;
qdog++){qbar[QFRED*q2+qdog]*=QBAD;}}}q9=fabs(qfoobar[QFRED])
+fabs(q4[QFRED]);qBuG=gmax(qBuG,q9);}for(QFRED=qfobar-1;
QFRED>=0;QFRED--){if(QFRED!=qfobar-1){if(q8!=0.0){for(qcat=
QQUUX;qcat<qfobar;qcat++){Q3[qcat*q2+QFRED]=(qbar[QFRED*q2+
qcat]/qbar[QFRED*q2+QQUUX])/q8;}for(qcat=QQUUX;qcat<qfobar;
qcat++){qsilly=0.0;for(qdog=QQUUX;qdog<qfobar;qdog++){qsilly
+=qbar[QFRED*q2+qdog]*Q3[qdog*q2+qcat];}for(qdog=QQUUX;qdog<
qfobar;qdog++){Q3[qdog*q2+qcat]+=qsilly*Q3[qdog*q2+QFRED];}}
}for(qcat=QQUUX;qcat<qfobar;qcat++){Q3[QFRED*q2+qcat]=0.0;Q3
[qcat*q2+QFRED]=0.0;}}Q3[QFRED*q2+QFRED]=1.0;q8=q4[QFRED];
QQUUX=QFRED;}for(QFRED=qfobar-1;QFRED>=0;QFRED--){QQUUX=
QFRED+1;q8=qfoobar[QFRED];if(QFRED!=qfobar-1){for(qcat=QQUUX
;qcat<qfobar;qcat++){qbar[QFRED*q2+qcat]=0.0;}}if(q8!=0.0){
if(QFRED!=qfobar-1){for(qcat=QQUUX;qcat<qfobar;qcat++){
qsilly=0.0;for(qdog=QQUUX;qdog<Q0;qdog++){qsilly+=qbar[qdog*
q2+QFRED]*qbar[qdog*q2+qcat];}QMUM=(qsilly/qbar[QFRED*q2+
QFRED])/q8;for(qdog=QFRED;qdog<Q0;qdog++){qbar[qdog*q2+qcat]
+=QMUM*qbar[qdog*q2+QFRED];}}}for(qcat=QFRED;qcat<Q0;qcat++)
{qbar[qcat*q2+QFRED]/=q8;}}else{for(qcat=QFRED;qcat<Q0;qcat
++){qbar[qcat*q2+QFRED]=0.0;}}qbar[QFRED*q2+QFRED]+=1.0;}for
(qdog=qfobar-1;qdog>=0;qdog--){QFISH=qdog-1;for(QgASp=1;
QgASp<=qfoobaz;QgASp++){q7=TRUE;for(QQUUX=qdog;QQUUX>=0;
QQUUX--){Q5=QQUUX-1;if(qBuG+fabs(q4[QQUUX])==qBuG){q7=FALSE;
break;}if(qBuG+fabs(qfoobar[Q5])==qBuG)break;}if(q7){qsilly=
1.0;for(QFRED=QQUUX;QFRED<=qdog;QFRED++){QMUM=qsilly*q4[
QFRED];if(qBuG+fabs(QMUM)==qBuG)break;q8=qfoobar[QFRED];qDAd
=qfoo(QMUM,q8);qfoobar[QFRED]=qDAd;Q10=q8/qDAd;qsilly=-QMUM/
qDAd;for(qcat=0;qcat<Q0;qcat++){Q11=qbar[qcat*q2+Q5];q12=
qbar[qcat*q2+QFRED];qbar[qcat*q2+Q5]=Q11*Q10+q12*qsilly;qbar
[qcat*q2+QFRED]=-Q11*qsilly+q12*Q10;}}}q12=qfoobar[qdog];if(
QQUUX==qdog){if(q12<0.0){qfoobar[qdog]=-q12;for(qcat=0;qcat<
qfobar;qcat++){Q3[qcat*q2+qdog]*=-1.0;}}break;}else{if(QgASp
>=qfoobaz){*qfOBAz=qdog+1;}QBUGGY=qfoobar[QQUUX];Q11=qfoobar
[QFISH];q8=q4[QFISH];qDAd=q4[qdog];QMUM=((Q11-q12)*(Q11+q12)
+(q8-qDAd)*(q8+qDAd))/(2.0*qDAd*Q11);q8=(fabs(QMUM)<=1e15)?
qfoo(QMUM,1.0):fabs(QMUM);QMUM=((QBUGGY-q12)*(QBUGGY+q12)+
qDAd*(Q11/(QMUM+dsign(q8,QMUM))-qDAd))/QBUGGY;Q10=1.0;qsilly
=1.0;for(Q6=QQUUX;Q6<=QFISH;Q6++){QFRED=Q6+1;q8=q4[QFRED];
Q11=qfoobar[QFRED];qDAd=qsilly*q8;q8=Q10*q8;q12=qfoo(QMUM,
qDAd);q4[Q6]=q12;if(q12!=0.0){Q10=QMUM/q12;qsilly=qDAd/q12;}
else{Q10=1.0;qsilly=0.0;}QMUM=QBUGGY*Q10+q8*qsilly;q8=-
QBUGGY*qsilly+q8*Q10;qDAd=Q11*qsilly;Q11=Q11*Q10;for(qcat=0;
qcat<qfobar;qcat++){QBUGGY=Q3[qcat*q2+Q6];q12=Q3[qcat*q2+
QFRED];Q3[qcat*q2+Q6]=QBUGGY*Q10+q12*qsilly;Q3[qcat*q2+QFRED
]=-QBUGGY*qsilly+q12*Q10;}q12=qfoo(QMUM,qDAd);qfoobar[Q6]=
q12;if(q12!=0.0){Q10=QMUM/q12;qsilly=qDAd/q12;}QMUM=Q10*q8+
qsilly*Q11;QBUGGY=-qsilly*q8+Q10*Q11;for(qcat=0;qcat<Q0;qcat
++){Q11=qbar[qcat*q2+Q6];q12=qbar[qcat*q2+QFRED];qbar[qcat*
q2+Q6]=Q11*Q10+q12*qsilly;qbar[qcat*q2+QFRED]=-Q11*qsilly+
q12*Q10;}}q4[QQUUX]=0.0;q4[qdog]=QMUM;qfoobar[qdog]=QBUGGY;}
}}}}double qfoo(double qbar,double qbaz){double Q13,Q14,
qfoobar;Q13=fabs(qbar);Q14=fabs(qbaz);if(Q13>Q14){qfoobar=
Q13;Q13=Q14;Q14=qfoobar;}if(Q14==0.0){return 0.0;}else{
qfoobar=Q13/Q14;return(Q14*sqrt(1.0+qfoobar*qfoobar));}}
