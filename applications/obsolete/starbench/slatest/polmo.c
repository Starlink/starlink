/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmpolmo(double qfoo,double qbar,double qbaz,double Q0,
double*qfobar,double*phi,double*q1){double q2,qfoobar,Q3,q4,
qfOBAz,qfoobaz,QQUUX,Q5,QFRED,qdog,qcat,QFISH,QgASp,Q6,q7,q8
,QBAD,qBuG,qsilly,QBUGGY;q2=sin(qfoo);qfoobar=cos(qfoo);Q3=
sin(qbar);q4=cos(qbar);qfOBAz=qfoobar*q4;qfoobaz=q2*q4;QQUUX
=Q3;qcat=sin(qbaz);QFISH=cos(qbaz);QgASp=sin(Q0);Q6=cos(Q0);
q7=(-qfoobaz*QgASp+QQUUX*Q6);q8=qfOBAz*QFISH-q7*qcat;QBAD=
qfoobaz*Q6+QQUUX*QgASp;qBuG=qfOBAz*qcat+q7*QFISH;Q5=-qcat*Q6
;QFRED=QgASp;qdog=QFISH*Q6;q4=sqrt(q8*q8+QBAD*QBAD);if(q4==
0.0)q8=1.0;q2=QBAD/q4;qfoobar=q8/q4;*qfobar=(q8!=0.0||QBAD!=
0.0)?atan2(QBAD,q8):0.0;*phi=atan2(qBuG,q4);qsilly=(Q5*
qfoobar+QFRED*q2)*qBuG-qdog*q4;QBUGGY=-Q5*q2+QFRED*qfoobar;*
q1=(qsilly!=0.0||QBUGGY!=0.0)?atan2(-QBUGGY,-qsilly):0.0;
return;}
