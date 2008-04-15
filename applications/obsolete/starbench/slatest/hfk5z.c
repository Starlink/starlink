/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmhfk5z(double rh,double qfoo,double qbar,double*qbaz,
double*Q0,double*qfobar,double*q1)
#define q2 0.484813681109535994e-5
{static double qfoobar[3]={-19.9e-3*q2,-9.1e-3*q2,22.9e-3*q2
},Q3[3]={-0.30e-3*q2,0.60e-3*q2,0.70e-3*q2};double q4[3],
qfOBAz[3][3],qfoobaz[3],QQUUX,Q5[3],QFRED[3][3],qdog[3][3],
qcat[6],QFISH[3],QgASp,Q6,q7;int q8;sbmdcs2c(rh,qfoo,q4);
sbmdav2m(qfoobar,qfOBAz);sbmdmxv(qfOBAz,Q3,qfoobaz);QQUUX=
qbar-2000.0;for(q8=0;q8<3;q8++){Q5[q8]=Q3[q8]*QQUUX;}
sbmdav2m(Q5,QFRED);sbmdmxm(qfOBAz,QFRED,qdog);sbmdimxv(qdog,
q4,qcat);sbmdvxv(qfoobaz,q4,QFISH);sbmdimxv(qdog,QFISH,qcat+
3);sbmdc62s(qcat,&QgASp,Q0,&Q6,qfobar,q1,&q7);*qbaz=
sbmdranrm(QgASp);}
