/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmue2el(double qfoo[],int qbar,int*qbaz,double*Q0,
double*qfobar,double*q1,double*q2,double*qfoobar,double*Q3,
double*q4,double*qfOBAz,int*qfoobaz)
#define QQUUX 0.01720209895
#define Q5 (QQUUX/86400.0)
{int QFRED;double qdog,qcat,QFISH[6];qdog=qfoo[0]-1.0;qcat=
qfoo[2];for(QFRED=0;QFRED<3;QFRED++){QFISH[QFRED]=qfoo[QFRED
+3];QFISH[QFRED+3]=qfoo[QFRED+6]*Q5;}sbmpv2el(QFISH,qcat,
qdog,qbar,qbaz,Q0,qfobar,q1,q2,qfoobar,Q3,q4,qfOBAz,qfoobaz)
;}
