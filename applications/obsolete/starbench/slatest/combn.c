/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmcombn(int qfoo,int qbar,int qbaz[],int*Q0){int
qfobar,q1,q2,qfoobar;if(qfoo<1||qbar<1||qfoo>qbar){*Q0=-1;
return;}else{*Q0=0;}if(qbaz[0]<1){for(qfobar=0;qfobar<qfoo;
qfobar++){qbaz[qfobar]=qfobar+1;}}else{qfobar=0;q1=1;while(
q1){if(qfobar==qfoo-1){q2=qbar+1;}else{q2=qbaz[qfobar+1];}if
(q2-qbaz[qfobar]>1){qbaz[qfobar]++;for(qfoobar=0;qfoobar<
qfobar;qfoobar++){qbaz[qfoobar]=qfoobar+1;}q1=0;}else{if(
qfobar==qfoo-1){*Q0=1;for(qfobar=0;qfobar<qfoo;qfobar++){
qbaz[qfobar]=qfobar+1;}q1=0;}else{qfobar++;}}}}}
