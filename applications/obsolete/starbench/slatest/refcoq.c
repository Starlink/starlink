/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmrefcoq(double qfoo,double qbar,double rh,double wl,
double*refa,double*refb){int qbaz;double Q0,p,qfobar,q1,q2,
qfoobar,Q3,q4,qfOBAz,qfoobaz;qbaz=(wl<=100.0);Q0=gmax(qfoo,
100.0);Q0=gmin(Q0,500.0);p=gmax(qbar,0.0);p=gmin(p,10000.0);
qfobar=gmax(rh,0.0);qfobar=gmin(qfobar,1.0);q1=gmax(wl,0.1);
q1=gmin(q1,1e6);if(p>0.0){q2=Q0-273.15;qfoobar=pow(10.0,(
0.7859+0.03477*q2)/(1.0+0.00412*q2))*(1.0+p*(4.5e-6+6e-10*q2
*q2));Q3=qfobar*qfoobar/(1.0-(1.0-qfobar)*qfoobar/p);}else{
Q3=0.0;}if(qbaz){q4=q1*q1;qfOBAz=((77.53484e-6+(4.39108e-7+
3.666e-9/q4)/q4)*p-11.2684e-6*Q3)/Q0;}else{qfOBAz=(
77.6890e-6*p-(6.3938e-6-0.375463/Q0)*Q3)/Q0;}qfoobaz=
4.4474e-6*Q0;if(!qbaz)qfoobaz-=0.0074*Q3*qfoobaz;*refa=
qfOBAz*(1.0-qfoobaz);*refb=-qfOBAz*(qfoobaz-qfOBAz/2.0);}
