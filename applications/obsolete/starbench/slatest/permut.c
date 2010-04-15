/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmpermut(int qfoo,int qbar[],int qbaz[],int*Q0){int
qfobar,q1,q2,qfoobar;if(qfoo<1){*Q0=-1;return;}else{*Q0=0;}
if(qbar[0]<0){qbar[0]=-1;for(qfobar=1;qfobar<qfoo;qfobar++){
qbar[qfobar]=0;}}qbar[0]++;for(qfobar=0;qfobar<qfoo;qfobar++
){q1=qfobar+1;if(qbar[qfobar]>=q1){qbar[qfobar]=0;if(q1>=
qfoo){*Q0=1;}else{qbar[q1]++;}}}for(qfobar=0;qfobar<qfoo;
qfobar++){qbaz[qfobar]=1;}for(qfobar=qfoo-1;qfobar>0;qfobar
--){q2=-1;for(qfoobar=0;qfoobar<=qbar[qfobar];qfoobar++){q2
++;while(qbaz[q2]>1){q2++;}}qbaz[q2]=qfobar+1;}}
