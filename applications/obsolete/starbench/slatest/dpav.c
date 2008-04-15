/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
double sbmdpav(double qfoo[3],double qbar[3]){double qbaz,Q0
,qfobar,q1,q2,qfoobar,Q3,q4,qfOBAz;qbaz=qfoo[0];Q0=qfoo[1];
qfobar=qfoo[2];q1=sqrt(qbaz*qbaz+Q0*Q0+qfobar*qfobar);if(q1
!=0.0){qbaz/=q1;Q0/=q1;qfobar/=q1;}q2=qbar[0];qfoobar=qbar[1
];Q3=qbar[2];q4=qfoobar*qbaz-q2*Q0;qfOBAz=Q3*(qbaz*qbaz+Q0*
Q0)-qfobar*(q2*qbaz+qfoobar*Q0);return(q4!=0.0||qfOBAz!=0.0)
?atan2(q4,qfOBAz):0.0;}
