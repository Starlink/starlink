/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmfk45z(double qfoo,double qbar,double qbaz,double*Q0,
double*qfobar){double q1;int q2,qfoobar;double Q3[3],q4[3],
qfOBAz[3],qfoobaz[6];static double QQUUX=100.0*60.0*60.0*
360.0/D2PI;static double Q5[3]={-1.62557e-6,-0.31919e-6,-
0.13843e-6};static double QFRED[3]={1.245e-3,-1.580e-3,-
0.659e-3};static double qdog[6][3]={{0.9999256782,-
0.0111820611,-0.0048579477},{0.0111820610,0.9999374784,-
0.0000271765},{0.0048579479,-0.0000271474,0.9999881997},{-
0.000551,-0.238565,0.435739},{0.238514,-0.002667,-0.008541},
{-0.435623,0.012254,0.002117}};sbmdcs2c(qfoo,qbar,Q3);q1=(
qbaz-1950.0)/QQUUX;for(q2=0;q2<3;q2++){q4[q2]=Q5[q2]+q1*
QFRED[q2];}q1=Q3[0]*q4[0]+Q3[1]*q4[1]+Q3[2]*q4[2];for(q2=0;
q2<3;q2++){qfOBAz[q2]=Q3[q2]-q4[q2]+q1*Q3[q2];}for(q2=0;q2<6
;q2++){q1=0.0;for(qfoobar=0;qfoobar<3;qfoobar++){q1+=qdog[q2
][qfoobar]*qfOBAz[qfoobar];}qfoobaz[q2]=q1;}q1=(sbmepj(
sbmepb2d(qbaz))-2000.0)/QQUUX;for(q2=0;q2<3;q2++){qfoobaz[q2
]+=q1*qfoobaz[q2+3];}sbmdcc2s(qfoobaz,&q1,qfobar);*Q0=
sbmdranrm(q1);}
