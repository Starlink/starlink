#if !defined( PAL_INCLUDED )  /* Include this file only once */
#define PAL_INCLUDED
/*
*  Name:
*     pal.c

*  Purpose:
*     A wrapper for all PAL functions used by AST.

*  Description:
*     This file is a wrapper that includes all the source files from
*     the PAL and SOFA libraries.  This is done so that the function
*     names can be changed from the usual "palXxx" and "iauXxx" forms to
*     the private "astPalXxx" and "astIauXxx" forms. This renaming is
*     performed via the macros defined in the pal2ast.h and sofa2ast.h
*     include files. This is done in order to prevent clashes with any
*     external PAL or SOFA libraries with which an application is linked.
*
*     This file expects to find the publicly distributed and unmodified
*     PAL and SOFA source code in a pair of subdirectory called "pal" and
*     "sofa" within the current directory. The files within these
*     subdirectories do not need to be compiled - only this wrapper file
*     needs to be compiled.

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public Licence as
*     published by the Free Software Foundation; either version 2 of
*     the Licence, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public Licence for more details.
*
*     You should have received a copy of the GNU General Public Licence
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: D.S. Berry (JAC, Hawaii)

*  History:
*     16-FEB-2012 (DSB):
*        Original version.
*/

/* Include configuration results in order to get any definition for the
   EXTERNAL_PAL macro. This macro is set if the --with-external_pal
   option was set when AST was configured. */
#if HAVE_CONFIG_H
#include <config.h>
#endif

/* If an external PAL library is being used, we leave this file empty. */
#ifndef EXTERNAL_PAL

/* Rename all PAL functions so that references to "palXxx" in the files
   included below get translated to "astPalXxx". */
#include "pal2ast.h"

/* Rename all SOFA functions so that references to "iauXxx" in the files
   included below get translated to "astIauXxx". */
#include "sofa2ast.h"

/* Include code from all PAL source files */
#include "pal/palAddet.c"
#include "pal/palAmpqk.c"
#include "pal/palCaldj.c"
#include "pal/palDat.c"
#include "pal/palDe2h.c"
#include "pal/palDeuler.c"
#include "pal/palDh2e.c"
#include "pal/palDjcal.c"
#include "pal/palDmat.c"
#include "pal/palDs2tp.c"
#include "pal/palDtp2s.c"
#include "pal/palDtps2c.c"
#include "pal/palDtt.c"
#include "pal/palEcmat.c"
#include "pal/palEqgal.c"
#include "pal/palEtrms.c"
#include "pal/palEvp.c"
#include "pal/palFk45z.c"
#include "pal/palFk524.c"
#include "pal/palFk54z.c"
#include "pal/palGaleq.c"
#include "pal/palGalsup.c"
#include "pal/palMappa.c"
#include "pal/palMapqkz.c"
#include "pal/palOne2One.c"
#include "pal/palPrebn.c"
#include "pal/palPrec.c"
#include "pal/palPrenut.c"
#include "pal/palPvobs.c"
#include "pal/palRvgalc.c"
#include "pal/palRvlg.c"
#include "pal/palRvlsrd.c"
#include "pal/palRvlsrk.c"
#include "pal/palSubet.c"
#include "pal/palSupgal.c"

/* Include code from all SOFA source files */
#include "sofa/a2af.c"
#include "sofa/a2tf.c"
#include "sofa/af2a.c"
#include "sofa/anp.c"
#include "sofa/anpm.c"
#include "sofa/bi00.c"
#include "sofa/bp00.c"
#include "sofa/bp06.c"
#include "sofa/bpn2xy.c"
#include "sofa/c2i00a.c"
#include "sofa/c2i00b.c"
#include "sofa/c2i06a.c"
#include "sofa/c2ibpn.c"
#include "sofa/c2ixy.c"
#include "sofa/c2ixys.c"
#include "sofa/c2s.c"
#include "sofa/c2t00a.c"
#include "sofa/c2t00b.c"
#include "sofa/c2t06a.c"
#include "sofa/c2tcio.c"
#include "sofa/c2teqx.c"
#include "sofa/c2tpe.c"
#include "sofa/c2txy.c"
#include "sofa/cal2jd.c"
#include "sofa/cp.c"
#include "sofa/cpv.c"
#include "sofa/cr.c"
#include "sofa/d2dtf.c"
#include "sofa/d2tf.c"
#include "sofa/dat.c"
#include "sofa/dtdb.c"
#include "sofa/dtf2d.c"
#include "sofa/ee00.c"
#include "sofa/ee00a.c"
#include "sofa/ee00b.c"
#include "sofa/ee06a.c"
#include "sofa/eect00.c"
#include "sofa/eform.c"
#include "sofa/eo06a.c"
#include "sofa/eors.c"
#include "sofa/epb.c"
#include "sofa/epb2jd.c"
#include "sofa/epj.c"
#include "sofa/epj2jd.c"
#include "sofa/epv00.c"
#include "sofa/eqeq94.c"
#include "sofa/era00.c"
#include "sofa/fad03.c"
#include "sofa/fae03.c"
#include "sofa/faf03.c"
#include "sofa/faju03.c"
#include "sofa/fal03.c"
#include "sofa/falp03.c"
#include "sofa/fama03.c"
#include "sofa/fame03.c"
#include "sofa/fane03.c"
#include "sofa/faom03.c"
#include "sofa/fapa03.c"
#include "sofa/fasa03.c"
#include "sofa/faur03.c"
#include "sofa/fave03.c"
#include "sofa/fk52h.c"
#include "sofa/fk5hip.c"
#include "sofa/fk5hz.c"
#include "sofa/fw2m.c"
#include "sofa/fw2xy.c"
#include "sofa/gc2gd.c"
#include "sofa/gc2gde.c"
#include "sofa/gd2gc.c"
#include "sofa/gd2gce.c"
#include "sofa/gmst00.c"
#include "sofa/gmst06.c"
#include "sofa/gmst82.c"
#include "sofa/gst00a.c"
#include "sofa/gst00b.c"
#include "sofa/gst06.c"
#include "sofa/gst06a.c"
#include "sofa/gst94.c"
#include "sofa/h2fk5.c"
#include "sofa/hfk5z.c"
#include "sofa/ir.c"
#include "sofa/jd2cal.c"
#include "sofa/jdcalf.c"
#include "sofa/num00a.c"
#include "sofa/num00b.c"
#include "sofa/num06a.c"
#include "sofa/numat.c"
#include "sofa/nut00a.c"
#include "sofa/nut00b.c"
#include "sofa/nut06a.c"
#include "sofa/nut80.c"
#include "sofa/nutm80.c"
#include "sofa/obl06.c"
#include "sofa/obl80.c"
#include "sofa/p06e.c"
#include "sofa/p2pv.c"
#include "sofa/p2s.c"
#include "sofa/pap.c"
#include "sofa/pas.c"
#include "sofa/pb06.c"
#include "sofa/pdp.c"
#include "sofa/pfw06.c"
#include "sofa/plan94.c"
#include "sofa/pm.c"
#include "sofa/pmat00.c"
#include "sofa/pmat06.c"
#include "sofa/pmat76.c"
#include "sofa/pmp.c"
#include "sofa/pn.c"
#include "sofa/pn00.c"
#include "sofa/pn00a.c"
#include "sofa/pn00b.c"
#include "sofa/pn06.c"
#include "sofa/pn06a.c"
#include "sofa/pnm00a.c"
#include "sofa/pnm00b.c"
#include "sofa/pnm06a.c"
#include "sofa/pnm80.c"
#include "sofa/pom00.c"
#include "sofa/ppp.c"
#include "sofa/ppsp.c"
#include "sofa/pr00.c"
#include "sofa/prec76.c"
#include "sofa/pv2p.c"
#include "sofa/pv2s.c"
#include "sofa/pvdpv.c"
#include "sofa/pvm.c"
#include "sofa/pvmpv.c"
#include "sofa/pvppv.c"
#include "sofa/pvstar.c"
#include "sofa/pvu.c"
#include "sofa/pvup.c"
#include "sofa/pvxpv.c"
#include "sofa/pxp.c"
#include "sofa/rm2v.c"
#include "sofa/rv2m.c"
#include "sofa/rx.c"
#include "sofa/rxp.c"
#include "sofa/rxpv.c"
#include "sofa/rxr.c"
#include "sofa/ry.c"
#include "sofa/rz.c"
#include "sofa/s00.c"
#include "sofa/s00a.c"
#include "sofa/s00b.c"
#include "sofa/s06.c"
#include "sofa/s06a.c"
#include "sofa/s2c.c"
#include "sofa/s2p.c"
#include "sofa/s2pv.c"
#include "sofa/s2xpv.c"
#include "sofa/sepp.c"
#include "sofa/seps.c"
#include "sofa/sp00.c"
#include "sofa/starpm.c"
#include "sofa/starpv.c"
#include "sofa/sxp.c"
#include "sofa/sxpv.c"
#include "sofa/taitt.c"
#include "sofa/taiut1.c"
#include "sofa/taiutc.c"
#include "sofa/tcbtdb.c"
#include "sofa/tcgtt.c"
#include "sofa/tdbtcb.c"
#include "sofa/tdbtt.c"
#include "sofa/tf2a.c"
#include "sofa/tf2d.c"
#include "sofa/tr.c"
#include "sofa/trxp.c"
#include "sofa/trxpv.c"
#include "sofa/tttai.c"
#include "sofa/tttcg.c"
#include "sofa/tttdb.c"
#include "sofa/ttut1.c"
#include "sofa/ut1tai.c"
#include "sofa/ut1tt.c"
#include "sofa/ut1utc.c"
#include "sofa/utctai.c"
#include "sofa/utcut1.c"
#include "sofa/xy06.c"
#include "sofa/xys00a.c"
#include "sofa/xys00b.c"
#include "sofa/xys06a.c"
#include "sofa/zp.c"
#include "sofa/zpv.c"
#include "sofa/zr.c"

#endif

#endif
