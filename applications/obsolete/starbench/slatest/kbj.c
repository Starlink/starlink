/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmkbj(int qfoo,double qbar,char*qbaz,int*Q0){*Q0=0;if(
qfoo==1){*qbaz='B';}else if(qfoo==2){*qbaz='J';}else if(qfoo
==0){if(qbar<1984.0){*qbaz='B';}else{*qbaz='J';}}else{*qbaz=
' ';*Q0=1;}}
