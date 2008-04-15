/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmue2pv(double qfoo,double qbar[],double qbaz[],int*Q0
)
#define qfobar 0.01720209895
#define q1 (qfobar/86400.0)
#define q2 1e-13
#define qfoobar 25
{int Q3,q4,qfOBAz;double qfoobaz,QQUUX,Q5,QFRED[3],qdog[3],
qcat,QFISH,QgASp,Q6,q7,q8,QBAD,qBuG,qsilly,QBUGGY,QMUM,qDAd,
q9,Q10,Q11,q12,Q13=0.0,Q14=0.0,qdisk,Q15,q16,q17;qfoobaz=
qbar[0];QQUUX=qbar[1];Q5=qbar[2];for(Q3=0;Q3<3;Q3++){QFRED[
Q3]=qbar[Q3+3];qdog[Q3]=qbar[Q3+6];}qcat=qbar[9];QFISH=qbar[
10];QgASp=qbar[11];Q6=qbar[12];Q6=Q6+(qfoo-QgASp)*qfobar/
qcat;q7=(qfoo-Q5)*qfobar;q4=1;q8=1.0;QBAD=0.0;while(fabs(q8)
>=QBAD){qfOBAz=0;qBuG=Q6;qsilly=qBuG*qBuG;QBUGGY=QQUUX*
qsilly;while(fabs(QBUGGY)>0.7){qfOBAz++;QBUGGY/=4.0;qBuG/=
2.0;qsilly/=4.0;}Q10=qBuG*qsilly*((((((QBUGGY/210.0+1.0)*
QBUGGY/156.0+1.0)*QBUGGY/110.0+1.0)*QBUGGY/72.0+1.0)*QBUGGY/
42.0+1.0)*QBUGGY/20.0+1.0)/6.0;q9=qsilly*((((((QBUGGY/182.0+
1.0)*QBUGGY/132.0+1.0)*QBUGGY/90.0+1.0)*QBUGGY/56.0+1.0)*
QBUGGY/30.0+1.0)*QBUGGY/12.0+1.0)/2.0;qDAd=qBuG+QQUUX*Q10;
QMUM=1.0+QQUUX*q9;QBAD=q2;while(qfOBAz>0){Q10=2.0*(QMUM*Q10+
qBuG*q9);q9=2.0*qDAd*qDAd;qDAd=2.0*QMUM*qDAd;QMUM=2.0*QMUM*
QMUM-1.0;qBuG+=qBuG;QBAD+=QBAD;qfOBAz--;}Q11=qcat*qDAd+QFISH
*q9+qfoobaz*Q10-q7;q12=qcat*QMUM+QFISH*qDAd+qfoobaz*q9;if(q4
==1)Q13=Q11;if(Q11*Q13<0.0){q8=Q11*(Q14-Q6)/(Q13-Q11);}else{
if(q12==0.0){*Q0=-1;return;}q8=Q11/q12;}Q14=Q6;Q13=Q11;Q6-=
q8;if(q4>qfoobar){*Q0=-2;return;}q4++;}q8=qfoobaz*q9;qdisk=
1.0-q8/qcat;Q15=q7-qfoobaz*Q10;q16=-qfoobaz*qDAd/(qcat*q12);
q17=1.0-q8/q12;for(Q3=0;Q3<3;Q3++){qbaz[Q3]=QFRED[Q3]*qdisk+
qdog[Q3]*Q15;qbaz[Q3+3]=q1*(QFRED[Q3]*q16+qdog[Q3]*q17);}
qbar[11]=qfoo;qbar[12]=Q6;*Q0=0;}
