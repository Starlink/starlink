/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmoapqk(char*qfoo,double qbar,double qbaz,double Q0[14
],double*qfobar,double*q1){static double q2=0.242535625;char
 qfoobar;double Q3,q4,sphi,cphi,qfOBAz,qfoobaz,QQUUX,Q5,
QFRED,qdog[3],qcat,QFISH,QgASp,Q6,q7,q8,QBAD,qBuG,qsilly,
QBUGGY,QMUM,qDAd,q9,Q10,Q11,diurab,q12,Q13;qfoobar=*qfoo;Q3=
qbar;q4=qbaz;sphi=Q0[1];cphi=Q0[2];qfOBAz=Q0[13];if(qfoobar
=='r'||qfoobar=='R'){qfoobar='R';}else if(qfoobar=='h'||
qfoobar=='H'){qfoobar='H';}else{qfoobar='A';}if(qfoobar=='A'
){qfoobaz=sin(q4);QQUUX=-cos(Q3)*qfoobaz;Q5=sin(Q3)*qfoobaz;
QFRED=cos(q4);}else{if(qfoobar=='R')Q3=qfOBAz-Q3;sbmdcs2c(-
Q3,q4,qdog);qcat=qdog[0];QFISH=qdog[1];QgASp=qdog[2];QQUUX=
sphi*qcat-cphi*QgASp;Q5=QFISH;QFRED=cphi*qcat+sphi*QgASp;}Q6
=(QQUUX!=0.0||Q5!=0.0)?atan2(Q5,QQUUX):0.0;q7=sqrt(QQUUX*
QQUUX+Q5*Q5);q8=atan2(q7,QFRED);if(QFRED>=q2){QBAD=q7/QFRED;
qBuG=(Q0[10]+Q0[11]*QBAD*QBAD)*QBAD;}else{sbmrefro(q8,Q0[4],
Q0[5],Q0[6],Q0[7],Q0[8],Q0[0],Q0[9],1e-8,&qBuG);}qsilly=q8+
qBuG;qfoobaz=sin(qsilly);QBUGGY=cos(Q6)*qfoobaz;QMUM=sin(Q6)
*qfoobaz;qDAd=cos(qsilly);q9=sphi*QBUGGY+cphi*qDAd;Q10=QMUM;
Q11=-cphi*QBUGGY+sphi*qDAd;diurab=-Q0[3];q12=1.0-diurab*Q10;
qdog[0]=q12*q9;qdog[1]=q12*(Q10+diurab);qdog[2]=q12*Q11;
sbmdcc2s(qdog,&Q13,q1);*qfobar=sbmdranrm(qfOBAz+Q13);}
