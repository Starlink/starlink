/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmprebn(double qfoo,double qbar,double qbaz[3][3]){
double Q0,qfobar,q1,q2,qfoobar,Q3,q4;Q0=(qfoo-1850.0)/100.0;
qfobar=(qbar-qfoo)/100.0;q1=qfobar*DAS2R;q2=2303.5548+(
1.39720+0.000059*Q0)*Q0;qfoobar=(q2+(0.30242-0.000269*Q0+
0.017996*qfobar)*qfobar)*q1;Q3=(q2+(1.09478+0.000387*Q0+
0.018324*qfobar)*qfobar)*q1;q4=(2005.1125+(-0.85294-0.000365
*Q0)*Q0+(-0.42647-0.000365*Q0-0.041802*qfobar)*qfobar)*q1;
sbmdeuler("\132\131\132",-qfoobar,q4,-Q3,qbaz);}
