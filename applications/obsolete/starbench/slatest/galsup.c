/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmgalsup(double qfoo,double qbar,double*qbaz,double*Q0
){double qfobar[3],q1[3];static double q2[3][3]={{-
0.735742574804,0.677261296414,0.0},{-0.074553778365,-
0.080991471307,0.993922590400},{0.673145302109,
0.731271165817,0.110081262225}};sbmdcs2c(qfoo,qbar,qfobar);
sbmdmxv(q2,qfobar,q1);sbmdcc2s(q1,qbaz,Q0);*qbaz=sbmdranrm(*
qbaz);*Q0=sbmdrange(*Q0);}
