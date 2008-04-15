/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmh2fk5(double rh,double qfoo,double qbar,double qbaz,
double*Q0,double*qfobar,double*q1,double*q2)
#define qfoobar 0.484813681109535994e-5
{static double Q3[3]={-19.9e-3*qfoobar,-9.1e-3*qfoobar,
22.9e-3*qfoobar},q4[3]={-0.30e-3*qfoobar,0.60e-3*qfoobar,
0.70e-3*qfoobar};double qfOBAz[6],qfoobaz[3][3],QQUUX[3],Q5[
3],QFRED[6],qdog,qcat,QFISH;int QgASp;sbmds2c6(rh,qfoo,1.0,
qbar,qbaz,0.0,qfOBAz);sbmdav2m(Q3,qfoobaz);sbmdmxv(qfoobaz,
q4,QQUUX);sbmdimxv(qfoobaz,qfOBAz,QFRED);sbmdvxv(qfOBAz,
QQUUX,Q5);for(QgASp=0;QgASp<3;QgASp++){Q5[QgASp]=qfOBAz[
QgASp+3]-Q5[QgASp];}sbmdimxv(qfoobaz,Q5,QFRED+3);sbmdc62s(
QFRED,&qdog,qfobar,&qcat,q1,q2,&QFISH);*Q0=sbmdranrm(qdog);}
