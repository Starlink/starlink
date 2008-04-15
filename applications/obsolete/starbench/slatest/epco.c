/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
#include <ctype.h>
double sbmepco(char qfoo,char qbar,double qbaz){double Q0;
int qfobar;qfobar=toupper((int)qfoo);if(qfobar==toupper((int
)qbar)){Q0=qbaz;}else{if(qfobar==(int)'B'){Q0=sbmepb(
sbmepj2d(qbaz));}else{Q0=sbmepj(sbmepb2d(qbaz));}}return(Q0)
;}
