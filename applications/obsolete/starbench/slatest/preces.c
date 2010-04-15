/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
#include <ctype.h>
void sbmpreces(char qfoo[3],double qbar,double qbaz,double*
Q0,double*qfobar){double q1[3][3],q2[3],qfoobar[3];if((
toupper((int)qfoo[0])!='F')||(toupper((int)qfoo[1])!='K')||(
(int)qfoo[2]!='4'&&(int)qfoo[2]!='5')){*Q0=-99.0;*qfobar=-
99.0;}else{if((int)qfoo[2]=='4')sbmprebn(qbar,qbaz,q1);else
sbmprec(qbar,qbaz,q1);sbmdcs2c(*Q0,*qfobar,q2);sbmdmxv(q1,q2
,qfoobar);sbmdcc2s(qfoobar,Q0,qfobar);*Q0=sbmdranrm(*Q0);}}
