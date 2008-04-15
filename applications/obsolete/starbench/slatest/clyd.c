/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmclyd(int qfoo,int qbar,int qbaz,int*Q0,int*qfobar,
int*q1){int q2;long qfoobar,Q3,q4,qfOBAz,qfoobaz,QQUUX,Q5;
static int QFRED[12]={31,28,31,30,31,30,31,31,30,31,30,31};
if(qfoo<-4711){*q1=1;return;}if((qbar<1)||(qbar>12)){*q1=2;
return;}q2=((qbar==2)&&(qfoo%4==0)&&(qfoo%100!=0||qfoo%400==
0))?1:0;*q1=(qbaz<1||qbaz>(QFRED[qbar-1]+q2))?3:0;QQUUX=(
long)qfoo;Q5=(long)qbar;qfoobar=(14-Q5)/12L;q4=QQUUX-qfoobar
;Q3=(1461L*(q4+4800L))/4L+(367L*(Q5-2L+12L*qfoobar))/12L-(3L
*((q4+4900L)/100L))/4L+(long)qbaz-30660L;q4=(Q3-1L)/1461L;
qfOBAz=Q3-1461L*q4;qfoobaz=(qfOBAz-1L)/365L-qfOBAz/1461L;Q3=
((80L*(qfOBAz-365L*qfoobaz+30L))/2447L)/11L;qfoobar=qfoobaz+
Q3;*qfobar=59+(int)(qfOBAz-365L*qfoobar+((4L-qfoobaz)/4L)*(
1L-Q3));*Q0=(int)(4L*q4+qfoobar)-4716;}
