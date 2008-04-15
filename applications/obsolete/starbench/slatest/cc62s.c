/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmcc62s(float qfoo[6],float*qbar,float*qbaz,float*Q0,
float*qfobar,float*q1,float*q2){double qfoobar,Q3,q4,qfOBAz,
qfoobaz,QQUUX,Q5,QFRED,qdog,qcat,QFISH;qfoobar=qfoo[0];Q3=
qfoo[1];q4=qfoo[2];qfOBAz=qfoo[3];qfoobaz=qfoo[4];QQUUX=qfoo
[5];Q5=qfoobar*qfoobar+Q3*Q3;if((qdog=Q5+q4*q4)==0.0){
qfoobar=qfOBAz;Q3=qfoobaz;q4=QQUUX;Q5=qfoobar*qfoobar+Q3*Q3;
qdog=Q5+q4*q4;}QFRED=sqrt(Q5);qcat=qfoobar*qfOBAz+Q3*qfoobaz
;if(Q5!=0.0){*qbar=(float)atan2(Q3,qfoobar);*qbaz=(float)
atan2(q4,QFRED);*qfobar=(float)((qfoobar*qfoobaz-Q3*qfOBAz)/
Q5);*q1=(float)((QQUUX*Q5-q4*qcat)/(qdog*QFRED));}else{*qbar
=0.0f;*qbaz=(float)((q4!=0.0)?atan2(q4,QFRED):0.0);*qfobar=
0.0f;*q1=0.0f;}*Q0=(float)(QFISH=sqrt(qdog));*q2=(float)((
QFISH!=0.0)?(qcat+q4*QQUUX)/QFISH:0.0);}
