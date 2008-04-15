/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmmxv(float qfoo[3][3],float qbar[3],float qbaz[3]){
int Q0,qfobar;float q1,q2[3];for(qfobar=0;qfobar<3;qfobar++)
{q1=0.0f;for(Q0=0;Q0<3;Q0++){q1+=qfoo[qfobar][Q0]*qbar[Q0];}
q2[qfobar]=q1;}for(qfobar=0;qfobar<3;qfobar++){qbaz[qfobar]=
q2[qfobar];}}
