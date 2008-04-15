/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmetrms(double qfoo,double qbar[3]){double qbaz,Q0,
qfobar,p,q1,q2;qbaz=(qfoo-1950.0)*1.00002135903e-2;Q0=
0.01673011-(0.00004193+0.000000126*qbaz)*qbaz;qfobar=(
84404.836-(46.8495+(0.00319+0.00181*qbaz)*qbaz)*qbaz)*DAS2R;
p=(1015489.951+(6190.67+(1.65+0.012*qbaz)*qbaz)*qbaz)*DAS2R;
q1=Q0*20.49552*DAS2R;q2=cos(p);qbar[0]=q1*sin(p);qbar[1]=-q1
*q2*cos(qfobar);qbar[2]=-q1*q2*sin(qfobar);}
