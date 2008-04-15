/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmcldj(int qfoo,int qbar,int qbaz,double*Q0,int*qfobar
){int q1;long q2,qfoobar;static int Q3[12]={31,28,31,30,31,
30,31,31,30,31,30,31};if(qfoo<-4699){*qfobar=1;return;}if((
qbar<1)||(qbar>12)){*qfobar=2;return;}q1=((qbar==2)&&(qfoo%4
==0)&&(qfoo%100!=0||qfoo%400==0))?1:0;*qfobar=(qbaz<1||qbaz>
(Q3[qbar-1]+q1))?3:0;q2=(long)qfoo;qfoobar=(long)qbar;*Q0=(
double)((1461L*(q2-(12L-qfoobar)/10L+4712L))/4L+(306L*((
qfoobar+9L)%12L)+5L)/10L-(3L*((q2-(12L-qfoobar)/10L+4900L)/
100L))/4L+(long)qbaz-2399904L);}
