/*
 *  This file is part of moggy.
 *
 *  Copyright 2001, 2003, Council for the Central Laboratory of the Research Councils
 *
 *  This program is part of the Starlink Software Distribution: see
 *  http://www.starlink.ac.uk
 *
 *  moggy is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  moggy is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with moggy; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 *  The General Public License is distributed along with this
 *  program in the file LICENCE.
 *
 *  Author: Norman Gray <norman@astro.gla.ac.uk>
 *  $Id$
 */

/* Small fragments of code to make up for the mildly filleted version
   of SkyCat libcat which this program uses.

   SkyCat's libcat (confusingly distinct from Starlink's libcat!) includes
   both some of Mark Calabretta's WCS code (wcslib) and some of Pat Wallace's
   SLALIB code, however these conflict with code included in Starlink
   libast_wcslib and libast_slalib.

   No problem: I can create a custom SkyCat libcat without the
   problematic code and link against that (see ../makefile).  That
   fails, however, because of the way that wcslib and slalib have been
   integrated into these particular AST glue libraries.

   The SkyCat library libcat.a includes modules proj.o and cel.o,
   which are from some oldish version of wcslib.  The latter refers to
   functions glsfwd and glsrev, which, as I see from AST's wcsmap.c,
   have been renamed sflfwd and sflrev in newer versions (it's changed
   in AST 1.8-1, but isn't in the version of AST with ast.h dated
   2000-02-08), so if we are to use such newer versions, we must
   provide a translation routine.  So that we can continue to use this
   route with older versions of AST, include these translations only
   if HAVE_GLSFWD is defined.  The libcat that we actually link
   against is edited within ../makefile to try to make sure that
   there's no duplication of functions (see config/configure.in).

   Note (not a propos of very much) that wcslib functions azpfwd,
   azprev, etc, have been renamed astAzpfwd, astAzprev, etc, in later
   versions of AST (post-1.8-1), and these AST versions (using a
   slightly different context struct -- AstPrjPrm instead of wcslib's
   prjprm) can be deemed to have diverged from the wcslib routines.
   This case is easy, and we don't need to do anything special,
   because the proj.o modules within libskycat can be used (ie, it
   appears that we do not need to link against specifically
   libwcslib.a).

   $Id$ */

/* struct prjprm is defined textually in SkyCat's
   catlib-3.7/astrotcl/wcslib/src/proj.c, and a prototype is in
   catlib-3.7/astrotcl/wcslib/include/wcslib.h.  */
#include <astrotcl/wcslib.h>

#include <config.h>

/* #ifndef HAVE_GLSFWD */
/* (1) Remap glsfwd and glsref to newer sflfwd and sflrev */
/* See WCS/AST proj.c */
int sflfwd (double phi, double theta,
	    struct prjprm *prj,
	    double *x, double *y);
int glsfwd (double phi, double theta,
	    struct prjprm *prj,
	    double *x, double *y)
{
    return sflfwd (phi, theta, prj, x, y);
}

int sflrev (double x, double y,
	    struct prjprm *prj,
	    double *phi, double* theta);
int glsrev (double x, double y,
	    struct prjprm *prj,
	    double *phi, double* theta)
{
    return sflrev (x, y, prj, phi, theta);
}
/* #endif /* #ifndef HAVE_GLSFWD */ */
