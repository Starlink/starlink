/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
#include <limits.h>
void sbmint2in(char*qfoo,int*qbar,int*qbaz,int*Q0){long
qfobar;qfobar=(long)*qbaz;sbmintin(qfoo,qbar,&qfobar,Q0);if(
*Q0<2){if(qfobar>=(long)INT_MIN&&qfobar<=(long)INT_MAX){*
qbaz=(int)qfobar;}else{*Q0=2;}}}
