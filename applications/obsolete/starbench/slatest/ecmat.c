/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmecmat(double qfoo,double qbar[3][3]){double qbaz,Q0;
qbaz=(qfoo-51544.5)/36525.0;Q0=DAS2R*(84381.448+(-46.8150+(-
0.00059+0.001813*qbaz)*qbaz)*qbaz);sbmdeuler("\130",Q0,0.0,
0.0,qbar);}
