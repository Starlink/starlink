/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
#include <string.h>
#include <limits.h>
static int qfoo(int,char*,int*,double*);void sbmintin(char*
qbar,int*qbaz,long*Q0,int*qfobar)
#define q1 0
#define q2 1
#define qfoobar 2
#define Q3 3
#define q4 4
#define qfOBAz 5
#define qfoobaz 6
{int QQUUX,Q5;double QFRED;int qdog;
#define qcat 100
#define QFISH 200
#define QgASp 300
#define Q6 400
#define q7 410
#define q8 1600
#define QBAD 1610
#define qBuG 1630
#define qsilly 1720
#define QBUGGY 9100
#define QMUM 9110
#define qDAd 9200
#define q9 9210
#define Q10 9900
int Q11;double q12;QQUUX=(int)strlen(qbar);Q5=*qbaz-1;q12=
0.0;Q11=0;qdog=qcat;while(qdog!=Q10){switch(qdog){case qcat:
switch(qfoo(QQUUX,qbar,&Q5,&QFRED)){case q1:qdog=Q6;break;
case q2:qdog=qcat;break;case qfoobar:qdog=QgASp;break;case
Q3:qdog=QFISH;break;case qfOBAz:qdog=QBUGGY;break;case q4:
case qfoobaz:qdog=QMUM;break;default:qdog=q9;}break;case
QFISH:Q11=-1;case QgASp:switch(qfoo(QQUUX,qbar,&Q5,&QFRED)){
case q1:qdog=Q6;break;case q2:qdog=QgASp;break;case qfoobar:
case Q3:case q4:case qfOBAz:qdog=qDAd;break;case qfoobaz:
default:qdog=q9;}break;case Q6:q12=q12*1e1+QFRED;qdog=(q12>=
(double)LONG_MIN&&q12<=(double)LONG_MAX)?q7:qDAd;break;case
q7:switch(qfoo(QQUUX,qbar,&Q5,&QFRED)){case q1:qdog=Q6;break
;case q2:qdog=QBAD;break;case qfoobar:case Q3:case q4:case
qfOBAz:qdog=q8;break;case qfoobaz:qdog=QBAD;break;default:
qdog=q9;}break;case q8:Q5--;case QBAD:if(Q11)q12=-q12;*Q0=(
long)(dnint(q12));case qBuG:switch(qfoo(QQUUX,qbar,&Q5,&
QFRED)){case q2:qdog=qBuG;break;case q1:case qfoobar:case Q3
:case qfOBAz:qdog=qsilly;break;case q4:case qfoobaz:qdog=Q10
;break;default:qdog=q9;}break;case qsilly:Q5--;qdog=Q10;
break;case QBUGGY:Q5--;case QMUM:Q11=1;qdog=Q10;break;case
qDAd:Q5--;case q9:Q11=2;qdog=Q10;break;default:qdog=q9;}}*
qbaz=Q5+1;*qfobar=Q11;}static int qfoo(int QQUUX,char*qbar,
int*Q5,double*QFRED){int Q13,Q14;char qdisk;
#define Q15 (15)
static char q16[Q15]={'0','1','2','3','4','5','6','7','8',
'9',' ','\t','+','-',','};static int q17[Q15]={q1,q1,q1,q1,
q1,q1,q1,q1,q1,q1,q2,q2,qfoobar,Q3,q4};Q13=qfOBAz;if(*Q5<0||
*Q5>=QQUUX){Q13=qfoobaz;}else{qdisk=qbar[*Q5];for(Q14=0;Q14<
Q15;Q14++){if(q16[Q14]==qdisk){Q13=q17[Q14];*QFRED=(double)
Q14;break;}}(*Q5)++;}return Q13;}
