/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmpxy(int qfoo,double qbar[][2],double qbaz[][2],
double Q0[6],double qfobar[][2],double*q1,double*q2,double*
qfoobar){int Q3;double q4,qfOBAz,qfoobaz,QQUUX,Q5,QFRED,qdog
,qcat,p;q4=0.0;qfOBAz=0.0;for(Q3=0;Q3<qfoo;Q3++){sbmxy2xy(
qbaz[Q3][0],qbaz[Q3][1],Q0,&qfoobaz,&QQUUX);qfobar[Q3][0]=
qfoobaz;qfobar[Q3][1]=QQUUX;Q5=qbar[Q3][0]-qfoobaz;QFRED=
qbar[Q3][1]-QQUUX;qdog=Q5*Q5;qcat=QFRED*QFRED;q4=q4+qdog;
qfOBAz=qfOBAz+qcat;}p=(double)gmax(1.0,qfoo);*q1=sqrt(q4/p);
*q2=sqrt(qfOBAz/p);*qfoobar=sqrt(*q1**q1+*q2**q2);}
