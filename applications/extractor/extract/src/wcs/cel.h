/*=============================================================================
*
*   WCSLIB - an implementation of the FITS WCS proposal.
*   Copyright (C) 1995-1999, Mark Calabretta
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
*   Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
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
*   $Id: cel.h,v 1.1.1.1 2002/03/15 16:33:26 bertin Exp $
*===========================================================================*/
 
#ifndef WCSLIB_CEL
#define WCSLIB_CEL

#include "proj.h"

#ifdef __cplusplus
extern "C" {
#endif

extern int npcode;
extern char pcodes[26][4];

struct celprm {
   int flag;
   double ref[4];
   double euler[5];

#if __STDC__  || defined(__cplusplus)
   int (*prjfwd)(const double, const double,
                 struct prjprm *,
                 double *, double *);
   int (*prjrev)(const double, const double,
                 struct prjprm *,
                 double *, double *);
#else
   int (*prjfwd)();
   int (*prjrev)();
#endif
};

#if __STDC__  || defined(__cplusplus)
   int celset(const char *, struct celprm *, struct prjprm *);
   int celfwd(const char *,
              const double, const double,
              struct celprm *,
              double *, double *,
              struct prjprm *,
              double *, double *);
   int celrev(const char *,
              const double, const double,
              struct prjprm *,
              double *, double *,
              struct celprm *,
              double *, double *);
#else
   int celset(), celfwd(), celrev();
#endif

extern const char *celset_errmsg[];
extern const char *celfwd_errmsg[];
extern const char *celrev_errmsg[];

#define CELSET 137

#ifdef __cplusplus
}
#endif

#endif /* WCSLIB_CEL */
