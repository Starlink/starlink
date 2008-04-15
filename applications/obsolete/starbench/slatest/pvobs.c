/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmpvobs(double p,double qfoo,double qbar,double qbaz[6
])
#define Q0 7.292115855306589e-5
{double qfobar,q1,q2,qfoobar,Q3;sbmgeoc(p,qfoo,&qfobar,&q1);
q2=sin(qbar);qfoobar=cos(qbar);Q3=Q0*qfobar;qbaz[0]=qfobar*
qfoobar;qbaz[1]=qfobar*q2;qbaz[2]=q1;qbaz[3]=-Q3*q2;qbaz[4]=
Q3*qfoobar;qbaz[5]=0.0;}
