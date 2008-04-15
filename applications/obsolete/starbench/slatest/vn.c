/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmvn(float qfoo[3],float qbar[3],float*qbaz){int Q0;
float qfobar,q1;qfobar=0.0f;for(Q0=0;Q0<3;Q0++){q1=qfoo[Q0];
qfobar=qfobar+q1*q1;}qfobar=(float)sqrt(qfobar);*qbaz=qfobar
;if(qfobar<=0.0f){qfobar=1.0f;}for(Q0=0;Q0<3;Q0++){qbar[Q0]=
qfoo[Q0]/qfobar;}}
