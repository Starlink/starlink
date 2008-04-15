/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdmxv(double qfoo[3][3],double qbar[3],double qbaz[3]
){int Q0,qfobar;double q1,q2[3];for(qfobar=0;qfobar<3;qfobar
++){q1=0.0;for(Q0=0;Q0<3;Q0++){q1+=qfoo[qfobar][Q0]*qbar[Q0]
;}q2[qfobar]=q1;}for(qfobar=0;qfobar<3;qfobar++){qbaz[qfobar
]=q2[qfobar];}}
