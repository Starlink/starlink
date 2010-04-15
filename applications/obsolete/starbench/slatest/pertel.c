/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmpertel(int qfoo,double qbar,double qbaz,double Q0,
double qfobar,double q1,double q2,double qfoobar,double Q3,
double q4,double*qfOBAz,double*qfoobaz,double*QQUUX,double*
Q5,double*QFRED,double*qdog,double*qcat,int*QFISH){double
QgASp[13],Q6;int q7,q8;if(qfoo<2||qfoo>3){*QFISH=-1;return;}
else{*QFISH=0;}sbmel2ue(qbar,qfoo,Q0,qfobar,q1,q2,qfoobar,Q3
,q4,0.0,QgASp,&q7);if(q7){*QFISH=q7;return;}sbmpertue(qbaz,
QgASp,&q7);if(q7>0){*QFISH=q7;}else if(q7<0){*QFISH=-5;
return;}sbmue2el(QgASp,qfoo,&q8,qfOBAz,qfoobaz,QQUUX,Q5,
QFRED,qdog,qcat,&Q6,&q7);if(q8!=qfoo||q7)*QFISH=-5;}
