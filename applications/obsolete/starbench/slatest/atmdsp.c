/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmatmdsp(double qfoo,double qbar,double rh,double qbaz
,double Q0,double qfobar,double q1,double*q2,double*qfoobar)
{double Q3,q4,qfOBAz,qfoobaz,QQUUX,Q5,QFRED,qdog,qcat,QFISH,
QgASp,Q6;if(qbaz>100.0||q1>100.0){*q2=Q0;*qfoobar=qfobar;}
else{Q3=gmax(qfoo,100.0);Q3=gmin(Q3,500.0);q4=gmax(qbar,0.0)
;q4=gmin(q4,10000.0);qfOBAz=gmax(rh,0.0);qfOBAz=gmin(qfOBAz,
1.0);qfoobaz=pow(10.0,-8.7115+0.03477*Q3);QQUUX=qfOBAz*
qfoobaz;Q5=11.2684e-6*QQUUX;QFRED=gmax(qbaz,0.1);qdog=QFRED*
QFRED;qcat=77.5317e-6+(0.43909e-6+0.00367e-6/qdog)/qdog;
QFISH=(qcat*q4-Q5)/Q3;QFRED=gmax(q1,0.1);qdog=QFRED*QFRED;
qcat=77.5317e-6+(0.43909e-6+0.00367e-6/qdog)/qdog;QgASp=(
qcat*q4-Q5)/Q3;if(QFISH!=0.0){Q6=QgASp/QFISH;*q2=Q0*Q6;*
qfoobar=qfobar*Q6;if(QFISH!=Q0)*qfoobar=*qfoobar*(1.0+QFISH*
(QFISH-QgASp)/(2.0*(QFISH-Q0)));}else{*q2=Q0;*qfoobar=qfobar
;}}}
