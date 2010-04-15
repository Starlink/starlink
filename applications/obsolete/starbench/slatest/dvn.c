/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdvn(double qfoo[3],double qbar[3],double*qbaz){int
Q0;double qfobar,q1;qfobar=0.0;for(Q0=0;Q0<3;Q0++){q1=qfoo[
Q0];qfobar+=q1*q1;}qfobar=sqrt(qfobar);*qbaz=qfobar;qfobar=(
qfobar>0.0)?qfobar:1.0;for(Q0=0;Q0<3;Q0++){qbar[Q0]=qfoo[Q0]
/qfobar;}}
