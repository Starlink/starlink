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
*     -  Changed names of all functions and structures to avoid name clashes 
*        with wcslib.
*=============================================================================
*/

#include "wcstrig.h"

struct AstPrjPar {
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

   int astAzpset(struct AstPrjPar *);
   int astAzpfwd(double, double, struct AstPrjPar *, double *, double *);
   int astAzprev(double, double, struct AstPrjPar *, double *, double *);
   int astTanset(struct AstPrjPar *);
   int astTanfwd(double, double, struct AstPrjPar *, double *, double *);
   int astTanrev(double, double, struct AstPrjPar *, double *, double *);
   int astSinset(struct AstPrjPar *);
   int astSinfwd(double, double, struct AstPrjPar *, double *, double *);
   int astSinrev(double, double, struct AstPrjPar *, double *, double *);
   int astStgset(struct AstPrjPar *);
   int astStgfwd(double, double, struct AstPrjPar *, double *, double *);
   int astStgrev(double, double, struct AstPrjPar *, double *, double *);
   int astArcset(struct AstPrjPar *);
   int astArcfwd(double, double, struct AstPrjPar *, double *, double *);
   int astArcrev(double, double, struct AstPrjPar *, double *, double *);
   int astZpnset(struct AstPrjPar *);
   int astZpnfwd(double, double, struct AstPrjPar *, double *, double *);
   int astZpnrev(double, double, struct AstPrjPar *, double *, double *);
   int astZeaset(struct AstPrjPar *);
   int astZeafwd(double, double, struct AstPrjPar *, double *, double *);
   int astZearev(double, double, struct AstPrjPar *, double *, double *);
   int astAirset(struct AstPrjPar *);
   int astAirfwd(double, double, struct AstPrjPar *, double *, double *);
   int astAirrev(double, double, struct AstPrjPar *, double *, double *);
   int astCypset(struct AstPrjPar *);
   int astCypfwd(double, double, struct AstPrjPar *, double *, double *);
   int astCyprev(double, double, struct AstPrjPar *, double *, double *);
   int astCarset(struct AstPrjPar *);
   int astCarfwd(double, double, struct AstPrjPar *, double *, double *);
   int astCarrev(double, double, struct AstPrjPar *, double *, double *);
   int astMerset(struct AstPrjPar *);
   int astMerfwd(double, double, struct AstPrjPar *, double *, double *);
   int astMerrev(double, double, struct AstPrjPar *, double *, double *);
   int astCeaset(struct AstPrjPar *);
   int astCeafwd(double, double, struct AstPrjPar *, double *, double *);
   int astCearev(double, double, struct AstPrjPar *, double *, double *);
   int astCopset(struct AstPrjPar *);
   int astCopfwd(double, double, struct AstPrjPar *, double *, double *);
   int astCoprev(double, double, struct AstPrjPar *, double *, double *);
   int astCodset(struct AstPrjPar *);
   int astCodfwd(double, double, struct AstPrjPar *, double *, double *);
   int astCodrev(double, double, struct AstPrjPar *, double *, double *);
   int astCoeset(struct AstPrjPar *);
   int astCoefwd(double, double, struct AstPrjPar *, double *, double *);
   int astCoerev(double, double, struct AstPrjPar *, double *, double *);
   int astCooset(struct AstPrjPar *);
   int astCoofwd(double, double, struct AstPrjPar *, double *, double *);
   int astCoorev(double, double, struct AstPrjPar *, double *, double *);
   int astBonset(struct AstPrjPar *);
   int astBonfwd(double, double, struct AstPrjPar *, double *, double *);
   int astBonrev(double, double, struct AstPrjPar *, double *, double *);
   int astPcoset(struct AstPrjPar *);
   int astPcofwd(double, double, struct AstPrjPar *, double *, double *);
   int astPcorev(double, double, struct AstPrjPar *, double *, double *);
   int astSflset(struct AstPrjPar *);
   int astSflfwd(double, double, struct AstPrjPar *, double *, double *);
   int astSflrev(double, double, struct AstPrjPar *, double *, double *);
   int astParset(struct AstPrjPar *);
   int astParfwd(double, double, struct AstPrjPar *, double *, double *);
   int astParrev(double, double, struct AstPrjPar *, double *, double *);
   int astAitset(struct AstPrjPar *);
   int astAitfwd(double, double, struct AstPrjPar *, double *, double *);
   int astAitrev(double, double, struct AstPrjPar *, double *, double *);
   int astMolset(struct AstPrjPar *);
   int astMolfwd(double, double, struct AstPrjPar *, double *, double *);
   int astMolrev(double, double, struct AstPrjPar *, double *, double *);
   int astCscset(struct AstPrjPar *);
   int astCscfwd(double, double, struct AstPrjPar *, double *, double *);
   int astCscrev(double, double, struct AstPrjPar *, double *, double *);
   int astQscset(struct AstPrjPar *);
   int astQscfwd(double, double, struct AstPrjPar *, double *, double *);
   int astQscrev(double, double, struct AstPrjPar *, double *, double *);
   int astTscset(struct AstPrjPar *);
   int astTscfwd(double, double, struct AstPrjPar *, double *, double *);
   int astTscrev(double, double, struct AstPrjPar *, double *, double *);

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
