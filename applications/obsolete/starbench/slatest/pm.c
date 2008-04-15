/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmpm(double qfoo,double qbar,double qbaz,double Q0,
double qfobar,double q1,double q2,double qfoobar,double*Q3,
double*q4){static double qfOBAz=(365.25*86400.0/149597870.0)
*DAS2R;int qfoobaz;double QQUUX,Q5[3],QFRED,p[3];sbmdcs2c(
qfoo,qbar,p);QQUUX=qfOBAz*q1*qfobar;Q5[0]=-qbaz*p[1]-Q0*cos(
qfoo)*sin(qbar)+QQUUX*p[0];Q5[1]=qbaz*p[0]-Q0*sin(qfoo)*sin(
qbar)+QQUUX*p[1];Q5[2]=Q0*cos(qbar)+QQUUX*p[2];QFRED=qfoobar
-q2;for(qfoobaz=0;qfoobaz<3;qfoobaz++){p[qfoobaz]+=QFRED*Q5[
qfoobaz];}sbmdcc2s(p,Q3,q4);*Q3=sbmdranrm(*Q3);}
