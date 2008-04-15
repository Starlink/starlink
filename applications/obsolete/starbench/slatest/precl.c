/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmprecl(double qfoo,double qbar,double qbaz[3][3]){
double Q0,qfobar,q1,q2,qfoobar,Q3,q4;Q0=(qfoo-2000.0)/1000.0
;qfobar=(qbar-qfoo)/1000.0;q1=qfobar*DAS2R;q2=23060.9097+(
139.7459+(-0.0038+(-0.5918+(-0.0037+0.0007*Q0)*Q0)*Q0)*Q0)*
Q0;qfoobar=(q2+(30.2226+(-0.2523+(-0.3840+(-0.0014+0.0007*Q0
)*Q0)*Q0)*Q0+(18.0183+(-0.1326+(0.0006+0.0005*Q0)*Q0)*Q0+(-
0.0583+(-0.0001+0.0007*Q0)*Q0+(-0.0285+(-0.0002)*qfobar)*
qfobar)*qfobar)*qfobar)*qfobar)*q1;Q3=(q2+(109.5270+(0.2446+
(-1.3913+(-0.0134+0.0026*Q0)*Q0)*Q0)*Q0+(18.2667+(-1.1400+(-
0.0173+0.0044*Q0)*Q0)*Q0+(-0.2821+(-0.0093+0.0032*Q0)*Q0+(-
0.0301+0.0006*Q0-0.0001*qfobar)*qfobar)*qfobar)*qfobar)*
qfobar)*q1;q4=(20042.0207+(-85.3131+(-0.2111+(0.3642+(0.0008
+(-0.0005)*Q0)*Q0)*Q0)*Q0)*Q0+(-42.6566+(-0.2111+(0.5463+(
0.0017+(-0.0012)*Q0)*Q0)*Q0)*Q0+(-41.8238+(0.0359+(0.0027+(-
0.0001)*Q0)*Q0)*Q0+(-0.0731+(0.0019+0.0009*Q0)*Q0+(-0.0127+
0.0011*Q0+0.0004*qfobar)*qfobar)*qfobar)*qfobar)*qfobar)*q1;
sbmdeuler("\132\131\132",-qfoobar,q4,-Q3,qbaz);}
