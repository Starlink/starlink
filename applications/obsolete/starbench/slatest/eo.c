/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
double sbmeo(double qfoo){double qbar[3][3];sbmpneqx(qfoo,
qbar);return sbmeors(qbar,sbms(qfoo,qbar[2][0],qbar[2][1]));
}
