/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmctf2d(int qfoo,int qbar,float qbaz,float*Q0,int*
qfobar)
#define q1 86400.0f
{*qfobar=0;if((qbaz<0.0f)||(qbaz>=60.0f)){*qfobar=3;return;}
if((qbar<0)||(qbar>59)){*qfobar=2;return;}if((qfoo<0)||(qfoo
>23)){*qfobar=1;return;}*Q0=(60.0f*(60.0f*(float)qfoo+(float
)qbar)+qbaz)/q1;}
