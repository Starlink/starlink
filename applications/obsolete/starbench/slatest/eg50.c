/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmeg50(double qfoo,double qbar,double*qbaz,double*Q0){
double qfobar[3],q1[3],q2,qfoobar;static double Q3[3][3]={{-
0.066988739415151,-0.872755765851993,-0.483538914632184},{
0.492728466075324,-0.450346958019961,0.744584633283031},{-
0.867600811151435,-0.188374601722920,0.460199784783852}};
sbmsubet(qfoo,qbar,1950.0,&q2,&qfoobar);sbmdcs2c(q2,qfoobar,
qfobar);sbmdmxv(Q3,qfobar,q1);sbmdcc2s(q1,qbaz,Q0);*qbaz=
sbmdranrm(*qbaz);*Q0=sbmdrange(*Q0);}
