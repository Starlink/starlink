#ifndef PROJ_INCLUDED
#define PROJ_INCLUDED
/*
*=============================================================================
*
*  This version of proj.h is based on the version in wcslib-2.2, but has
*  been modified in the following ways by the Starlink project (e-mail: 
*  ussc@star.rl.ac.uk):
*     -  Support for non-ANSI C prototypes removed
*        (D.S. Berry, 19th June 1996).
*     -  Changed the name of the PROJ macro to PROJ_INCLUDED
*        (R.F. Warren-Smith, 13th November 1996).
*     -  Un-define the PI macro if previously defined (R.F. Warren-Smith,
*        23rd April 1997).
*     -  Changed prjprm.p to use"PVj_m" instead of "PROJPi" ("double p[10]" 
*        changed to "double **p", w[10] changed to w[110], and np, unset,
*        axlat and axlon added). (D.S. Berry 1st April 2000).
*     -  Changed names of all function to avoid name clashes with wcslib.
*=============================================================================
*/

#include "wcstrig.h"

struct prjprm {
   int flag;
   int n;
   double r0;
   double **p;
   int *np;
   double w[110];
   double unset;
   int axlat;
   int axlon;
};

   int astAzpset(struct prjprm *);
   int astAzpfwd(double, double, struct prjprm *, double *, double *);
   int astAzprev(double, double, struct prjprm *, double *, double *);
   int astTanset(struct prjprm *);
   int astTanfwd(double, double, struct prjprm *, double *, double *);
   int astTanrev(double, double, struct prjprm *, double *, double *);
   int astSinset(struct prjprm *);
   int astSinfwd(double, double, struct prjprm *, double *, double *);
   int astSinrev(double, double, struct prjprm *, double *, double *);
   int astStgset(struct prjprm *);
   int astStgfwd(double, double, struct prjprm *, double *, double *);
   int astStgrev(double, double, struct prjprm *, double *, double *);
   int astArcset(struct prjprm *);
   int astArcfwd(double, double, struct prjprm *, double *, double *);
   int astArcrev(double, double, struct prjprm *, double *, double *);
   int astZpnset(struct prjprm *);
   int astZpnfwd(double, double, struct prjprm *, double *, double *);
   int astZpnrev(double, double, struct prjprm *, double *, double *);
   int astZeaset(struct prjprm *);
   int astZeafwd(double, double, struct prjprm *, double *, double *);
   int astZearev(double, double, struct prjprm *, double *, double *);
   int astAirset(struct prjprm *);
   int astAirfwd(double, double, struct prjprm *, double *, double *);
   int astAirrev(double, double, struct prjprm *, double *, double *);
   int astCypset(struct prjprm *);
   int astCypfwd(double, double, struct prjprm *, double *, double *);
   int astCyprev(double, double, struct prjprm *, double *, double *);
   int astCarset(struct prjprm *);
   int astCarfwd(double, double, struct prjprm *, double *, double *);
   int astCarrev(double, double, struct prjprm *, double *, double *);
   int astMerset(struct prjprm *);
   int astMerfwd(double, double, struct prjprm *, double *, double *);
   int astMerrev(double, double, struct prjprm *, double *, double *);
   int astCeaset(struct prjprm *);
   int astCeafwd(double, double, struct prjprm *, double *, double *);
   int astCearev(double, double, struct prjprm *, double *, double *);
   int astCopset(struct prjprm *);
   int astCopfwd(double, double, struct prjprm *, double *, double *);
   int astCoprev(double, double, struct prjprm *, double *, double *);
   int astCodset(struct prjprm *);
   int astCodfwd(double, double, struct prjprm *, double *, double *);
   int astCodrev(double, double, struct prjprm *, double *, double *);
   int astCoeset(struct prjprm *);
   int astCoefwd(double, double, struct prjprm *, double *, double *);
   int astCoerev(double, double, struct prjprm *, double *, double *);
   int astCooset(struct prjprm *);
   int astCoofwd(double, double, struct prjprm *, double *, double *);
   int astCoorev(double, double, struct prjprm *, double *, double *);
   int astBonset(struct prjprm *);
   int astBonfwd(double, double, struct prjprm *, double *, double *);
   int astBonrev(double, double, struct prjprm *, double *, double *);
   int astPcoset(struct prjprm *);
   int astPcofwd(double, double, struct prjprm *, double *, double *);
   int astPcorev(double, double, struct prjprm *, double *, double *);
   int astSflset(struct prjprm *);
   int astSflfwd(double, double, struct prjprm *, double *, double *);
   int astSflrev(double, double, struct prjprm *, double *, double *);
   int astParset(struct prjprm *);
   int astParfwd(double, double, struct prjprm *, double *, double *);
   int astParrev(double, double, struct prjprm *, double *, double *);
   int astAitset(struct prjprm *);
   int astAitfwd(double, double, struct prjprm *, double *, double *);
   int astAitrev(double, double, struct prjprm *, double *, double *);
   int astMolset(struct prjprm *);
   int astMolfwd(double, double, struct prjprm *, double *, double *);
   int astMolrev(double, double, struct prjprm *, double *, double *);
   int astCscset(struct prjprm *);
   int astCscfwd(double, double, struct prjprm *, double *, double *);
   int astCscrev(double, double, struct prjprm *, double *, double *);
   int astQscset(struct prjprm *);
   int astQscfwd(double, double, struct prjprm *, double *, double *);
   int astQscrev(double, double, struct prjprm *, double *, double *);
   int astTscset(struct prjprm *);
   int astTscfwd(double, double, struct prjprm *, double *, double *);
   int astTscrev(double, double, struct prjprm *, double *, double *);

#if defined( PI )
#undef PI
#endif
#define PI 3.141592653589793238462643
#define D2R PI/180.0
#define R2D 180.0/PI
#define SQRT2 1.4142135623730950488
#define SQRT2INV 1.0/SQRT2

#define PRJSET 137

#define NPTAN 40

#endif /* PROJ_INCLUDED */
