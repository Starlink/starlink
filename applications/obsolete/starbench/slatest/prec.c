/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmprec(double qfoo,double qbar,double qbaz[3][3]){
double Q0,qfobar,q1,q2,qfoobar,Q3,q4;Q0=(qfoo-2000.0)/100.0;
qfobar=(qbar-qfoo)/100.0;q1=qfobar*DAS2R;q2=2306.2181+((
1.39656-(0.000139*Q0))*Q0);qfoobar=(q2+((0.30188-0.000344*Q0
)+0.017998*qfobar)*qfobar)*q1;Q3=(q2+((1.09468+0.000066*Q0)+
0.018203*qfobar)*qfobar)*q1;q4=((2004.3109+(-0.85330-
0.000217*Q0)*Q0)+((-0.42665-0.000217*Q0)-0.041833*qfobar)*
qfobar)*q1;sbmdeuler("\132\131\132",-qfoobar,q4,-Q3,qbaz);}
