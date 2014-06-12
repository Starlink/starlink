/*=============================================================================
*
*   WCSLIB - an implementation of the FITS WCS proposal.
*   Copyright (C) 1995-2002, Mark Calabretta
*
*   This library is free software; you can redistribute it and/or modify it
*   under the terms of the GNU Library General Public License as published
*   by the Free Software Foundation; either version 2 of the License, or (at
*   your option) any later version.
*
*   This library is distributed in the hope that it will be useful, but
*   WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library
*   General Public License for more details.
*
*   You should have received a copy of the GNU Library General Public License
*   along with this library; if not, write to the Free Software Foundation,
*   Inc., 51 Franklin Street,Fifth Floor, Boston, MA 02110-1301, USA
*
*   Correspondence concerning WCSLIB may be directed to:
*      Internet email: mcalabre@atnf.csiro.au
*      Postal address: Dr. Mark Calabretta,
*                      Australia Telescope National Facility,
*                      P.O. Box 76,
*                      Epping, NSW, 2121,
*                      AUSTRALIA
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*   $Id$
*=============================================================================
*
*  This version of proj.h is based on the version in wcslib-2.9, but has
*  been modified in the following ways by the Starlink project (e-mail:
*  ussc@star.rl.ac.uk):
*     -  Support for non-ANSI C prototypes removed
*     -  Changed the name of the WCSLIB_PROJ macro to WCSLIB_PROJ_INCLUDED
*     -  Changed names of all functions and structures to avoid name
*        clashes with wcslib.
*     -  Change the maximum number of projection parameters to 100.
*     -  Added definition of macro WCSLIB_MXPAR, and use it to define
*        size of projection parameter array within AstPrjPrm structure.
*     -  Added component "p2" to the AstPrjPrm structure to hold projection
*        parameters associated with the longitude axis (for use within
*        the tpn.c file which holds an implementation of the old "TAN with
*        correction terms" projection).
*     -  Added prototypes for TPN projection functions (defined in file
*        tpn.c).
*     -  Added prototypes for HPX projection functions.
*     -  Added prototypes for XPH projection functions.
*===========================================================================*/

#ifndef WCSLIB_PROJ_INCLUDED
#define WCSLIB_PROJ_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif

#define WCSLIB_MXPAR 100

extern int npcode;
extern char pcodes[26][4];

struct AstPrjPrm {
   char   code[4];
   int    flag;
   double phi0, theta0;
   double r0;
   double *p;
   double *p2;
   double w[20];
   int    n;
   int (*astPRJfwd)(const double, const double,
                 struct AstPrjPrm *,
                 double *, double *);
   int (*astPRJrev)(const double, const double,
                 struct AstPrjPrm *,
                 double *, double *);
};

   int astPRJset(const char [], struct AstPrjPrm *);
   int astPRJfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astPRJrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astAZPset(struct AstPrjPrm *);
   int astAZPfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astAZPrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astSZPset(struct AstPrjPrm *);
   int astSZPfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astSZPrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astTANset(struct AstPrjPrm *);
   int astTANfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astTANrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astSTGset(struct AstPrjPrm *);
   int astSTGfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astSTGrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astSINset(struct AstPrjPrm *);
   int astSINfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astSINrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astARCset(struct AstPrjPrm *);
   int astARCfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astARCrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astZPNset(struct AstPrjPrm *);
   int astZPNfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astZPNrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astZEAset(struct AstPrjPrm *);
   int astZEAfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astZEArev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astAIRset(struct AstPrjPrm *);
   int astAIRfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astAIRrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astCYPset(struct AstPrjPrm *);
   int astCYPfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astCYPrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astCEAset(struct AstPrjPrm *);
   int astCEAfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astCEArev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astCARset(struct AstPrjPrm *);
   int astCARfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astCARrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astMERset(struct AstPrjPrm *);
   int astMERfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astMERrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astSFLset(struct AstPrjPrm *);
   int astSFLfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astSFLrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astPARset(struct AstPrjPrm *);
   int astPARfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astPARrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astMOLset(struct AstPrjPrm *);
   int astMOLfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astMOLrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astAITset(struct AstPrjPrm *);
   int astAITfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astAITrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astCOPset(struct AstPrjPrm *);
   int astCOPfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astCOPrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astCOEset(struct AstPrjPrm *);
   int astCOEfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astCOErev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astCODset(struct AstPrjPrm *);
   int astCODfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astCODrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astCOOset(struct AstPrjPrm *);
   int astCOOfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astCOOrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astBONset(struct AstPrjPrm *);
   int astBONfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astBONrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astPCOset(struct AstPrjPrm *);
   int astPCOfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astPCOrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astTSCset(struct AstPrjPrm *);
   int astTSCfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astTSCrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astCSCset(struct AstPrjPrm *);
   int astCSCfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astCSCrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astQSCset(struct AstPrjPrm *);
   int astQSCfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astQSCrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astHPXset(struct AstPrjPrm *);
   int astHPXfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astHPXrev(const double, const double, struct AstPrjPrm *, double *, double *);
   int astXPHset(struct AstPrjPrm *);
   int astXPHfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astXPHrev(const double, const double, struct AstPrjPrm *, double *, double *);

   int astTPNset(struct AstPrjPrm *);
   int astTPNfwd(const double, const double, struct AstPrjPrm *, double *, double *);
   int astTPNrev(const double, const double, struct AstPrjPrm *, double *, double *);

extern const char *astPRJset_errmsg[];
extern const char *astPRJfwd_errmsg[];
extern const char *astPRJrev_errmsg[];

#ifdef __cplusplus
};
#endif

#endif /* WCSLIB_PROJ_INCLUDED */
