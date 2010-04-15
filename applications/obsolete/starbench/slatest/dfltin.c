/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
#include <string.h>
static int qfoo(int,char*,int*,int*,double*);void sbmdfltin(
char*qbar,int*qbaz,double*Q0,int*qfobar)
#define q1 0
#define q2 1
#define qfoobar 2
#define Q3 3
#define q4 4
#define qfOBAz 5
#define qfoobaz 6
#define QQUUX 7
#define Q5 8
{int QFRED,qdog,qcat;double QFISH;int QgASp;
#define Q6 100
#define q7 200
#define q8 300
#define QBAD 400
#define qBuG 500
#define qsilly 600
#define QBUGGY 700
#define QMUM 800
#define qDAd 900
#define q9 1000
#define Q10 1100
#define Q11 1200
#define q12 1300
#define Q13 1310
#define Q14 1620
#define qdisk 1720
#define Q15 9100
#define q16 9110
#define q17 9200
#define QEMPTY 9210
#define q18 9900
int QFULL,qfast,qsmall,QBIG,QOK=0;double QHELLO;QFRED=(int)
strlen(qbar);qdog=*qbaz-1;QHELLO=0.0;QFULL=1;qfast=0;QBIG=1;
qsmall=0;QgASp=Q6;while(QgASp!=q18){switch(QgASp){case Q6:
switch(qfoo(QFRED,qbar,&qdog,&qcat,&QFISH)){case q1:QgASp=
QBAD;break;case q2:QgASp=Q6;break;case qfoobar:QgASp=QMUM;
break;case Q3:QgASp=qBuG;break;case q4:QgASp=q8;break;case
qfOBAz:QgASp=q7;break;case QQUUX:QgASp=Q15;break;case
qfoobaz:case Q5:QgASp=q16;break;default:QgASp=QEMPTY;}break;
case q7:QFULL=-1;case q8:switch(qfoo(QFRED,qbar,&qdog,&qcat,
&QFISH)){case q1:QgASp=QBAD;break;case q2:QgASp=q8;break;
case qfoobar:QgASp=QMUM;break;case Q3:QgASp=qBuG;break;case
q4:case qfOBAz:case qfoobaz:case QQUUX:QgASp=q17;break;case
Q5:default:QgASp=QEMPTY;}break;case QBAD:QHELLO=QHELLO*1e1+
QFISH;switch(qfoo(QFRED,qbar,&qdog,&qcat,&QFISH)){case q1:
QgASp=QBAD;break;case q2:QgASp=Q13;break;case qfoobar:QgASp=
qDAd;break;case Q3:QgASp=qsilly;break;case q4:case qfOBAz:
case qfoobaz:case QQUUX:QgASp=q12;break;case Q5:QgASp=Q13;
break;default:QgASp=QEMPTY;}break;case qBuG:switch(qfoo(
QFRED,qbar,&qdog,&qcat,&QFISH)){case q1:QgASp=QBUGGY;break;
case q2:QgASp=qBuG;break;case qfoobar:case Q3:case q4:case
qfOBAz:case qfoobaz:case QQUUX:QgASp=q17;break;case Q5:
default:QgASp=QEMPTY;}break;case qsilly:switch(qfoo(QFRED,
qbar,&qdog,&qcat,&QFISH)){case q1:QgASp=QBUGGY;break;case
qfoobar:QgASp=qDAd;break;case Q3:case q4:case qfOBAz:case
qfoobaz:case QQUUX:QgASp=q12;break;case q2:case Q5:QgASp=Q13
;break;default:QgASp=QEMPTY;}break;case QBUGGY:qsmall++;
QHELLO=QHELLO*1e1+QFISH;QgASp=qsilly;break;case QMUM:QHELLO=
1.0;case qDAd:switch(qfoo(QFRED,qbar,&qdog,&qcat,&QFISH)){
case q1:QgASp=Q11;break;case q2:QgASp=qDAd;break;case q4:
QgASp=Q10;break;case qfOBAz:QgASp=q9;break;case qfoobar:case
 Q3:case qfoobaz:case QQUUX:QgASp=q17;break;case Q5:default:
QgASp=QEMPTY;}break;case q9:QBIG=-1;case Q10:switch(qfoo(
QFRED,qbar,&qdog,&qcat,&QFISH)){case q1:QgASp=Q11;break;case
 q2:QgASp=Q10;break;case qfoobar:case Q3:case q4:case qfOBAz
:case qfoobaz:case QQUUX:QgASp=q17;break;case Q5:default:
QgASp=QEMPTY;}break;case Q11:qfast=qfast*10+qcat;if(qfast>
100){QgASp=q17;}else{switch(qfoo(QFRED,qbar,&qdog,&qcat,&
QFISH)){case q1:QgASp=Q11;break;case q2:QgASp=Q13;break;case
 qfoobar:case Q3:case q4:case qfOBAz:case qfoobaz:case QQUUX
:QgASp=q12;break;case Q5:QgASp=Q13;break;default:QgASp=
QEMPTY;}}break;case q12:qdog--;case Q13:qfast=qfast*QBIG-
qsmall;if(qfast>=0){while(qfast>=10){QHELLO*=1e10;qfast-=10;
}while(qfast>=1){QHELLO*=1e1;qfast--;}}else{while(qfast<=-10
){QHELLO/=1e10;qfast+=10;}while(qfast<=-1){QHELLO/=1e1;qfast
++;}}if(QFULL==1){*Q0=QHELLO;QOK=0;}else{*Q0=-QHELLO;QOK=-1;
}case Q14:switch(qfoo(QFRED,qbar,&qdog,&qcat,&QFISH)){case
q2:QgASp=Q14;break;case q1:case qfoobar:case Q3:case q4:case
 qfOBAz:case QQUUX:QgASp=qdisk;break;case qfoobaz:case Q5:
QgASp=q18;break;default:QgASp=QEMPTY;}break;case qdisk:qdog
--;QgASp=q18;break;case Q15:qdog--;case q16:QOK=1;QgASp=q18;
break;case q17:qdog--;case QEMPTY:QOK=2;QgASp=q18;break;
default:QgASp=QEMPTY;}}*qbaz=qdog+1;*qfobar=QOK;}static int
qfoo(int QFRED,char*qbar,int*qdog,int*qcat,double*QFISH){int
 QBYE,QMAGIC;char q19;
#define q20 (20)
static char qobSCUrE[q20]={'0','1','2','3','4','5','6','7',
'8','9',' ','\t','D','d','E','e','.','+','-',','};static int
 QSPEED[q20]={q1,q1,q1,q1,q1,q1,q1,q1,q1,q1,q2,q2,qfoobar,
qfoobar,qfoobar,qfoobar,Q3,q4,qfOBAz,qfoobaz};QBYE=QQUUX;if(
*qdog<0||*qdog>=QFRED){QBYE=Q5;}else{q19=qbar[*qdog];for(
QMAGIC=0;QMAGIC<q20;QMAGIC++){if(qobSCUrE[QMAGIC]==q19){QBYE
=QSPEED[QMAGIC];*qcat=QMAGIC;*QFISH=(double)*qcat;break;}}(*
qdog)++;}return QBYE;}
