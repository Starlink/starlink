/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdjcal(int qfoo,double qbar,int qbaz[4],int*Q0){
double qfobar,q1,q2,qfoobar;long Q3,q4,qfOBAz;if((qbar<=-
2395520.0)||(qbar>=1.0e9)){*Q0=-1;return;}else{qfobar=pow(
10.0,(double)gmax(qfoo,0));qfobar=dnint(qfobar);q1=qbar*
qfobar;q1=dnint(q1);q2=dmod(q1,qfobar);if(q2<0.0)q2+=qfobar;
qfoobar=(q1-q2)/qfobar;Q3=(long)dnint(qfoobar)+2400001L;q4=
4L*(Q3+((2L*((4L*Q3-17918L)/146097L)*3L)/4L+1L)/2L-37L);
qfOBAz=10L*(((q4-237L)%1461L)/4L)+5L;qbaz[0]=(int)((q4/1461L
)-4712L);qbaz[1]=(int)(((qfOBAz/306L+2L)%12L)+1L);qbaz[2]=(
int)((qfOBAz%306L)/10L+1L);qbaz[3]=(int)dnint(q2);*Q0=0;}}
