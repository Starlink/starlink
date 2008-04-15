/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmfw2xy(double qfoo,double qbar,double qbaz,double Q0,
double*qfobar,double*q1){double q2,qfoobar,Q3,q4,qfOBAz,
qfoobaz;q2=sin(qfoo);qfoobar=cos(qfoo);Q3=sin(qbaz);q4=sin(
Q0);qfOBAz=cos(qbaz)*cos(qbar);qfoobaz=cos(Q0)*sin(qbar);*
qfobar=q4*(Q3*qfoobar-qfOBAz*q2)+qfoobaz*q2;*q1=q4*(Q3*q2+
qfOBAz*qfoobar)-qfoobaz*qfoobar;}
