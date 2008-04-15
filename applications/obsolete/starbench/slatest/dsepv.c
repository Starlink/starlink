/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
double sbmdsepv(double qfoo[3],double qbar[3]){double qbaz[3
],Q0[3],qfobar,q1;sbmdvxv(qfoo,qbar,qbaz);sbmdvn(qbaz,Q0,&
qfobar);q1=sbmdvdv(qfoo,qbar);return(qfobar!=0.0||q1!=0.0)?
atan2(qfobar,q1):0.0;}
