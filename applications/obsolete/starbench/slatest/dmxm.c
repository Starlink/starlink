/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdmxm(double qfoo[3][3],double qbar[3][3],double qbaz
[3][3]){int Q0,qfobar,q1;double q2,qfoobar[3][3];for(Q0=0;Q0
<3;Q0++){for(qfobar=0;qfobar<3;qfobar++){q2=0.0;for(q1=0;q1<
3;q1++){q2+=qfoo[Q0][q1]*qbar[q1][qfobar];}qfoobar[Q0][
qfobar]=q2;}}for(qfobar=0;qfobar<3;qfobar++){for(Q0=0;Q0<3;
Q0++){qbaz[Q0][qfobar]=qfoobar[Q0][qfobar];}}}
