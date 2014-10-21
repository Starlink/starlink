#if !defined( PAL_INCLUDED )  /* Include this file only once */
#define PAL_INCLUDED
/*
*  Name:
*     pal.c

*  Purpose:
*     A wrapper for all PAL functions used by AST.

*  Description:
*     This file is a wrapper that includes all the source files from
*     the PAL and ERFA libraries.  This is done so that the function
*     names can be changed from the usual "palXxx" and "eraXxx" forms to
*     the private "astPalXxx" and "astIauXxx" forms. This renaming is
*     performed via the macros defined in the pal2ast.h and erfa2ast.h
*     include files. This is done in order to prevent clashes with any
*     external PAL or ERFA libraries with which an application is linked.
*
*     This file expects to find the publicly distributed and unmodified
*     PAL and ERFA source code in a pair of subdirectory called "pal" and
*     "erfa" within the current directory. The files within these
*     subdirectories do not need to be compiled - only this wrapper file
*     needs to be compiled.

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*     
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*     
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

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

/* Rename all ERFA functions so that references to "eraXxx" in the files
   included below get translated to "astEraXxx". */
#include "erfa2ast.h"

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
#include "pal/palDrange.c"
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
#include "pal/palGeoc.c"
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

/* Include code from all ERFA source files */
#include "erfa/a2af.c"
#include "erfa/a2tf.c"
#include "erfa/af2a.c"
#include "erfa/anp.c"
#include "erfa/anpm.c"
#include "erfa/bi00.c"
#include "erfa/bp00.c"
#include "erfa/bp06.c"
#include "erfa/bpn2xy.c"
#include "erfa/c2i00a.c"
#include "erfa/c2i00b.c"
#include "erfa/c2i06a.c"
#include "erfa/c2ibpn.c"
#include "erfa/c2ixy.c"
#include "erfa/c2ixys.c"
#include "erfa/c2s.c"
#include "erfa/c2t00a.c"
#include "erfa/c2t00b.c"
#include "erfa/c2t06a.c"
#include "erfa/c2tcio.c"
#include "erfa/c2teqx.c"
#include "erfa/c2tpe.c"
#include "erfa/c2txy.c"
#include "erfa/cal2jd.c"
#include "erfa/cp.c"
#include "erfa/cpv.c"
#include "erfa/cr.c"
#include "erfa/d2dtf.c"
#include "erfa/d2tf.c"
#include "erfa/dat.c"
#include "erfa/dtdb.c"
#include "erfa/dtf2d.c"
#include "erfa/ee00.c"
#include "erfa/ee00a.c"
#include "erfa/ee00b.c"
#include "erfa/ee06a.c"
#include "erfa/eect00.c"
#include "erfa/eform.c"
#include "erfa/eo06a.c"
#include "erfa/eors.c"
#include "erfa/epb.c"
#include "erfa/epb2jd.c"
#include "erfa/epj.c"
#include "erfa/epj2jd.c"
#include "erfa/epv00.c"
#include "erfa/eqeq94.c"
#include "erfa/era00.c"
#include "erfa/fad03.c"
#include "erfa/fae03.c"
#include "erfa/faf03.c"
#include "erfa/faju03.c"
#include "erfa/fal03.c"
#include "erfa/falp03.c"
#include "erfa/fama03.c"
#include "erfa/fame03.c"
#include "erfa/fane03.c"
#include "erfa/faom03.c"
#include "erfa/fapa03.c"
#include "erfa/fasa03.c"
#include "erfa/faur03.c"
#include "erfa/fave03.c"
#include "erfa/fk52h.c"
#include "erfa/fk5hip.c"
#include "erfa/fk5hz.c"
#include "erfa/fw2m.c"
#include "erfa/fw2xy.c"
#include "erfa/gc2gd.c"
#include "erfa/gc2gde.c"
#include "erfa/gd2gc.c"
#include "erfa/gd2gce.c"
#include "erfa/gmst00.c"
#include "erfa/gmst06.c"
#include "erfa/gmst82.c"
#include "erfa/gst00a.c"
#include "erfa/gst00b.c"
#include "erfa/gst06.c"
#include "erfa/gst06a.c"
#include "erfa/gst94.c"
#include "erfa/h2fk5.c"
#include "erfa/hfk5z.c"
#include "erfa/ir.c"
#include "erfa/jd2cal.c"
#include "erfa/jdcalf.c"
#include "erfa/num00a.c"
#include "erfa/num00b.c"
#include "erfa/num06a.c"
#include "erfa/numat.c"
#include "erfa/nut00a.c"
#include "erfa/nut00b.c"
#include "erfa/nut06a.c"
#include "erfa/nut80.c"
#include "erfa/nutm80.c"
#include "erfa/obl06.c"
#include "erfa/obl80.c"
#include "erfa/p06e.c"
#include "erfa/p2pv.c"
#include "erfa/p2s.c"
#include "erfa/pap.c"
#include "erfa/pas.c"
#include "erfa/pb06.c"
#include "erfa/pdp.c"
#include "erfa/pfw06.c"
#include "erfa/plan94.c"
#include "erfa/pm.c"
#include "erfa/pmat00.c"
#include "erfa/pmat06.c"
#include "erfa/pmat76.c"
#include "erfa/pmp.c"
#include "erfa/pn.c"
#include "erfa/pn00.c"
#include "erfa/pn00a.c"
#include "erfa/pn00b.c"
#include "erfa/pn06.c"
#include "erfa/pn06a.c"
#include "erfa/pnm00a.c"
#include "erfa/pnm00b.c"
#include "erfa/pnm06a.c"
#include "erfa/pnm80.c"
#include "erfa/pom00.c"
#include "erfa/ppp.c"
#include "erfa/ppsp.c"
#include "erfa/pr00.c"
#include "erfa/prec76.c"
#include "erfa/pv2p.c"
#include "erfa/pv2s.c"
#include "erfa/pvdpv.c"
#include "erfa/pvm.c"
#include "erfa/pvmpv.c"
#include "erfa/pvppv.c"
#include "erfa/pvstar.c"
#include "erfa/pvu.c"
#include "erfa/pvup.c"
#include "erfa/pvxpv.c"
#include "erfa/pxp.c"
#include "erfa/refco.c"
#include "erfa/rm2v.c"
#include "erfa/rv2m.c"
#include "erfa/rx.c"
#include "erfa/rxp.c"
#include "erfa/rxpv.c"
#include "erfa/rxr.c"
#include "erfa/ry.c"
#include "erfa/rz.c"
#include "erfa/s00.c"
#include "erfa/s00a.c"
#include "erfa/s00b.c"
#include "erfa/s06.c"
#include "erfa/s06a.c"
#include "erfa/s2c.c"
#include "erfa/s2p.c"
#include "erfa/s2pv.c"
#include "erfa/s2xpv.c"
#include "erfa/sepp.c"
#include "erfa/seps.c"
#include "erfa/sp00.c"
#include "erfa/starpm.c"
#include "erfa/starpv.c"
#include "erfa/sxp.c"
#include "erfa/sxpv.c"
#include "erfa/taitt.c"
#include "erfa/taiut1.c"
#include "erfa/taiutc.c"
#include "erfa/tcbtdb.c"
#include "erfa/tcgtt.c"
#include "erfa/tdbtcb.c"
#include "erfa/tdbtt.c"
#include "erfa/tf2a.c"
#include "erfa/tf2d.c"
#include "erfa/tr.c"
#include "erfa/trxp.c"
#include "erfa/trxpv.c"
#include "erfa/tttai.c"
#include "erfa/tttcg.c"
#include "erfa/tttdb.c"
#include "erfa/ttut1.c"
#include "erfa/ut1tai.c"
#include "erfa/ut1tt.c"
#include "erfa/ut1utc.c"
#include "erfa/utctai.c"
#include "erfa/utcut1.c"
#include "erfa/xy06.c"
#include "erfa/xys00a.c"
#include "erfa/xys00b.c"
#include "erfa/xys06a.c"
#include "erfa/zp.c"
#include "erfa/zpv.c"
#include "erfa/zr.c"

#endif

#endif
