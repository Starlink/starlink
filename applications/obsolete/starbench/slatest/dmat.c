/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdmat(int qfoo,double*qbar,double*qbaz,double*Q0,int*
qfobar,int*q1)
#define q2 1e-20
{int qfoobar,Q3,q4,qfOBAz,qfoobaz;double QQUUX,Q5,QFRED;
double*qdog,*qcat,*QFISH;*qfobar=0;*Q0=1.0;for(qfoobar=0,
qdog=qbar;qfoobar<qfoo;qfoobar++,qdog+=qfoo){QQUUX=fabs(qdog
[qfoobar]);Q3=qfoobar;QFISH=qdog;if(qfoobar!=qfoo){for(q4=
qfoobar+1,qcat=qdog+qfoo;q4<qfoo;q4++,qcat+=qfoo){Q5=fabs(
qcat[qfoobar]);if(Q5>QQUUX){QQUUX=Q5;Q3=q4;QFISH=qcat;}}}if(
QQUUX<q2){*qfobar=-1;}else{if(Q3!=qfoobar){for(qfOBAz=0;
qfOBAz<qfoo;qfOBAz++){Q5=qdog[qfOBAz];qdog[qfOBAz]=QFISH[
qfOBAz];QFISH[qfOBAz]=Q5;}Q5=qbaz[qfoobar];qbaz[qfoobar]=
qbaz[Q3];qbaz[Q3]=Q5;*Q0=-*Q0;}q1[qfoobar]=Q3;*Q0*=qdog[
qfoobar];if(fabs(*Q0)<q2){*qfobar=-1;}else{qdog[qfoobar]=1.0
/qdog[qfoobar];for(qfOBAz=0;qfOBAz<qfoo;qfOBAz++){if(qfOBAz
!=qfoobar){qdog[qfOBAz]*=qdog[qfoobar];}}QFRED=qbaz[qfoobar]
*qdog[qfoobar];qbaz[qfoobar]=QFRED;for(q4=0,qcat=qbar;q4<
qfoo;q4++,qcat+=qfoo){if(q4!=qfoobar){for(qfOBAz=0;qfOBAz<
qfoo;qfOBAz++){if(qfOBAz!=qfoobar){qcat[qfOBAz]-=qcat[
qfoobar]*qdog[qfOBAz];}}qbaz[q4]-=qcat[qfoobar]*QFRED;}}for(
q4=0,qcat=qbar;q4<qfoo;q4++,qcat+=qfoo){if(q4!=qfoobar){qcat
[qfoobar]*=-qdog[qfoobar];}}}}}if(*qfobar!=0){*Q0=0.0;}else{
for(qfoobar=qfoo;qfoobar-->0;){qfoobaz=q1[qfoobar];if(
qfoobar!=qfoobaz){for(q4=0,qcat=qbar;q4<qfoo;q4++,qcat+=qfoo
){Q5=qcat[qfoobar];qcat[qfoobar]=qcat[qfoobaz];qcat[qfoobaz]
=Q5;}}}}}
