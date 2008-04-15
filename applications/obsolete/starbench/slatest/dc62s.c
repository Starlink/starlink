/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdc62s(double qfoo[6],double*qbar,double*qbaz,double*
Q0,double*qfobar,double*q1,double*q2){double qfoobar,Q3,q4,
qfOBAz,qfoobaz,QQUUX,Q5,QFRED,qdog,qcat;qfoobar=qfoo[0];Q3=
qfoo[1];q4=qfoo[2];qfOBAz=qfoo[3];qfoobaz=qfoo[4];QQUUX=qfoo
[5];Q5=qfoobar*qfoobar+Q3*Q3;if((qdog=Q5+q4*q4)==0.0){
qfoobar=qfOBAz;Q3=qfoobaz;q4=QQUUX;Q5=qfoobar*qfoobar+Q3*Q3;
qdog=Q5+q4*q4;}QFRED=sqrt(Q5);qcat=qfoobar*qfOBAz+Q3*qfoobaz
;if(Q5!=0.0){*qbar=atan2(Q3,qfoobar);*qbaz=atan2(q4,QFRED);*
qfobar=(qfoobar*qfoobaz-Q3*qfOBAz)/Q5;*q1=(QQUUX*Q5-q4*qcat)
/(qdog*QFRED);}else{*qbar=0.0;*qbaz=(q4!=0.0)?atan2(q4,QFRED
):0.0;*qfobar=0.0;*q1=0.0;}*q2=((*Q0=sqrt(qdog))!=0.0)?(qcat
+q4*QQUUX)/(*Q0):0.0;}
