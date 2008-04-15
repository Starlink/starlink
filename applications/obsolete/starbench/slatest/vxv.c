/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmvxv(float qfoo[3],float qbar[3],float qbaz[3]){float
 Q0[3];int qfobar;Q0[0]=qfoo[1]*qbar[2]-qfoo[2]*qbar[1];Q0[1
]=qfoo[2]*qbar[0]-qfoo[0]*qbar[2];Q0[2]=qfoo[0]*qbar[1]-qfoo
[1]*qbar[0];for(qfobar=0;qfobar<3;qfobar++)qbaz[qfobar]=Q0[
qfobar];}
