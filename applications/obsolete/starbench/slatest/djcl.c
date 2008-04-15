/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdjcl(double qfoo,int*qbar,int*qbaz,int*Q0,double*
qfobar,int*q1){double q2,qfoobar;long Q3,q4,qfOBAz;if((qfoo
<=-2395520.0)||(qfoo>=1e9)){*q1=-1;return;}else{*q1=0;q2=
dmod(qfoo,1.0);if(q2<0.0)q2+=1.0;qfoobar=qfoo-q2;qfoobar=
dnint(qfoobar);Q3=(long)dnint(qfoobar)+2400001;q4=4L*(Q3+((
6L*((4L*Q3-17918L)/146097L))/4L+1L)/2L-37L);qfOBAz=10L*(((q4
-237L)%1461L)/4L)+5L;*qbar=(int)(q4/1461L-4712L);*qbaz=(int)
(((qfOBAz/306L+2L)%12L)+1L);*Q0=(int)((qfOBAz%306L)/10L+1L);
*qfobar=q2;*q1=0;}}
