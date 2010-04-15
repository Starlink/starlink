/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
float sbmsepv(float qfoo[3],float qbar[3]){int qbaz;double
Q0[3],qfobar[3];for(qbaz=0;qbaz<3;qbaz++){Q0[qbaz]=(double)
qfoo[qbaz];qfobar[qbaz]=(double)qbar[qbaz];}return(float)
sbmdsepv(Q0,qfobar);}
