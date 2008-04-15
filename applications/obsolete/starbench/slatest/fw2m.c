/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmfw2m(double qfoo,double qbar,double qbaz,double Q0,
double qfobar[3][3]){double q1,q2,sphi,cphi,qfoobar,Q3,q4,
qfOBAz;q1=sin(qfoo);q2=cos(qfoo);sphi=sin(qbar);cphi=cos(
qbar);qfoobar=sin(qbaz);Q3=cos(qbaz);q4=sin(Q0);qfOBAz=cos(
Q0);qfobar[0][0]=Q3*q2+qfoobar*cphi*q1;qfobar[0][1]=Q3*q1-
qfoobar*cphi*q2;qfobar[0][2]=-qfoobar*sphi;qfobar[1][0]=
qfOBAz*qfoobar*q2-(qfOBAz*Q3*cphi+q4*sphi)*q1;qfobar[1][1]=
qfOBAz*qfoobar*q1+(qfOBAz*Q3*cphi+q4*sphi)*q2;qfobar[1][2]=
qfOBAz*Q3*sphi-q4*cphi;qfobar[2][0]=q4*qfoobar*q2-(q4*Q3*
cphi-qfOBAz*sphi)*q1;qfobar[2][1]=q4*qfoobar*q1+(q4*Q3*cphi-
qfOBAz*sphi)*q2;qfobar[2][2]=q4*Q3*sphi+qfOBAz*cphi;}
