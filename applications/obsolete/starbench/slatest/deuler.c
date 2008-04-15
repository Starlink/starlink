/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
#include <string.h>
void sbmdeuler(char*qfoo,double phi,double qbar,double qbaz,
double Q0[3][3]){int qfobar,q1,q2,qfoobar,Q3;double q4[3][3]
,qfOBAz[3][3],qfoobaz,QQUUX,Q5,QFRED,qdog[3][3];char qcat;
for(qfobar=0;qfobar<3;qfobar++){for(q1=0;q1<3;q1++){q4[q1][
qfobar]=(q1==qfobar)?1.0:0.0;}}q2=(int)strlen(qfoo);for(
qfoobar=0;qfoobar<3;qfoobar++){if(qfoobar<=q2){for(qfobar=0;
qfobar<3;qfobar++){for(q1=0;q1<3;q1++){qfOBAz[q1][qfobar]=(
q1==qfobar)?1.0:0.0;}}switch(qfoobar){case 0:qfoobaz=phi;
break;case 1:qfoobaz=qbar;break;default:qfoobaz=qbaz;break;}
QQUUX=sin(qfoobaz);Q5=cos(qfoobaz);qcat=qfoo[qfoobar];if((
qcat=='X')||(qcat=='x')||(qcat=='1')){qfOBAz[1][1]=Q5;qfOBAz
[1][2]=QQUUX;qfOBAz[2][1]=-QQUUX;qfOBAz[2][2]=Q5;}else if((
qcat=='Y')||(qcat=='y')||(qcat=='2')){qfOBAz[0][0]=Q5;qfOBAz
[0][2]=-QQUUX;qfOBAz[2][0]=QQUUX;qfOBAz[2][2]=Q5;}else if((
qcat=='Z')||(qcat=='z')||(qcat=='3')){qfOBAz[0][0]=Q5;qfOBAz
[0][1]=QQUUX;qfOBAz[1][0]=-QQUUX;qfOBAz[1][1]=Q5;}else{q2=0;
}for(q1=0;q1<3;q1++){for(qfobar=0;qfobar<3;qfobar++){QFRED=
0.0;for(Q3=0;Q3<3;Q3++){QFRED+=qfOBAz[q1][Q3]*q4[Q3][qfobar]
;}qdog[q1][qfobar]=QFRED;}}for(qfobar=0;qfobar<3;qfobar++){
for(q1=0;q1<3;q1++){q4[q1][qfobar]=qdog[q1][qfobar];}}}}for(
qfobar=0;qfobar<3;qfobar++){for(q1=0;q1<3;q1++){Q0[q1][
qfobar]=q4[q1][qfobar];}}}
