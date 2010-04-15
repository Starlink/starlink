/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmpv2ue(double qfoo[],double qbar,double qbaz,double
Q0[],int*qfobar)
#define q1 0.01720209895
#define q2 (q1/86400.0);
#define qfoobar 1e-3
#define Q3 1e-3
{double q4,qfOBAz,qfoobaz,QQUUX,Q5,QFRED,qdog,qcat,QFISH,
QgASp,Q6,q7,q8;q4=qbar;if(qbaz<0.0){*qfobar=-1;return;}
qfOBAz=1.0+qbaz;qfoobaz=qfoo[0];QQUUX=qfoo[1];Q5=qfoo[2];
QFRED=qfoo[3]/q2;qdog=qfoo[4]/q2;qcat=qfoo[5]/q2;QFISH=sqrt(
qfoobaz*qfoobaz+QQUUX*QQUUX+Q5*Q5);QgASp=QFRED*QFRED+qdog*
qdog+qcat*qcat;Q6=sqrt(QgASp);if(QFISH<qfoobar){*qfobar=-2;
return;}if(Q6<Q3){*qfobar=-3;return;}q7=QgASp-2.0*qfOBAz/
QFISH;q8=qfoobaz*QFRED+QQUUX*qdog+Q5*qcat;Q0[0]=qfOBAz;Q0[1]
=q7;Q0[2]=q4;Q0[3]=qfoobaz;Q0[4]=QQUUX;Q0[5]=Q5;Q0[6]=QFRED;
Q0[7]=qdog;Q0[8]=qcat;Q0[9]=QFISH;Q0[10]=q8;Q0[11]=q4;Q0[12]
=0.0;*qfobar=0;}
