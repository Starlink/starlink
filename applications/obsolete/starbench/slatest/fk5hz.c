/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmfk5hz(double qfoo,double qbar,double qbaz,double*rh,
double*Q0)
#define qfobar 0.484813681109535994e-5
{static double q1[3]={-19.9e-3*qfobar,-9.1e-3*qfobar,22.9e-3
*qfobar},q2[3]={-0.30e-3*qfobar,0.60e-3*qfobar,0.70e-3*
qfobar};double qfoobar[3],Q3[3][3],q4,qfOBAz[3],qfoobaz[3][3
],QQUUX[3],Q5[3],QFRED;int qdog;sbmdcs2c(qfoo,qbar,qfoobar);
sbmdav2m(q1,Q3);q4=2000.0-qbaz;for(qdog=0;qdog<3;qdog++){
qfOBAz[qdog]=q2[qdog]*q4;}sbmdav2m(qfOBAz,qfoobaz);sbmdimxv(
qfoobaz,qfoobar,QQUUX);sbmdmxv(Q3,QQUUX,Q5);sbmdcc2s(Q5,&
QFRED,Q0);*rh=sbmdranrm(QFRED);}
