/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmeqgal(double qfoo,double qbar,double*qbaz,double*Q0)
{double qfobar[3],q1[3];static double q2[3][3]={{-
0.054875539695716,-0.873437107995315,-0.483834985836994},{
0.494109453305607,-0.444829589431879,0.746982251810510},{-
0.867666135847849,-0.198076386130820,0.455983795721093}};
sbmdcs2c(qfoo,qbar,qfobar);sbmdmxv(q2,qfobar,q1);sbmdcc2s(q1
,qbaz,Q0);*qbaz=sbmdranrm(*qbaz);*Q0=sbmdrange(*Q0);}
