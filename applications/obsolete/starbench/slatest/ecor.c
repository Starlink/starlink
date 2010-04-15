/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmecor(float qfoo,float qbar,int qbaz,int Q0,float
qfobar,float*q1,float*q2)
#define qfoobar 1.4959787066e8f
#define Q3 499.0047837f
{float q4[6],qfOBAz[3];sbmearth(qbaz,Q0,qfobar,q4);sbmcs2c(
qfoo,qbar,qfOBAz);*q1=-qfoobar*sbmvdv(&q4[3],qfOBAz);*q2=Q3*
sbmvdv(q4,qfOBAz);}
