/*
*+
*  Name:
*     pal1sofa.h

*  Purpose:
*     Mappings of ERFA names to SOFA names

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Include file

*  Invocation:
*     #include "pal1sofa.h"

*  Description:
*     PAL will work with both SOFA and ERFA libraries and the
*     difference is generally a change in prefix. This include
*     file maps the ERFA form of functions to the SOFA form
*     and includes the relevant sofa.h vs erfa.h file.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - PAL uses the ERFA form by default.

*  History:
*     2014-07-29 (TIMJ):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2014 Tim Jenness
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

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#ifndef PAL1SOFAHDEF
#define PAL1SOFAHDEF

#if HAVE_CONFIG_H
#  include <config.h>
#endif

# if HAVE_SOFA_H

#  include "sofa.h"
#  include "sofam.h"

   /* Must replace ERFA with SOFA */

#  define eraA2af iauA2af
#  define eraA2tf iauA2tf
#  define eraAf2a iauAf2a
#  define eraAnp iauAnp
#  define eraAnpm iauAnpm
#  define eraC2s iauC2s
#  define eraCal2jd iauCal2jd
#  define eraD2tf iauD2tf
#  define eraDat iauDat
#  define eraEe06a iauEe06a
#  define eraEpb iauEpb
#  define eraEpb2jd iauEpb2jd
#  define eraEpj iauEpj
#  define eraEpj2jd iauEpj2jd
#  define eraEpv00 iauEpv00
#  define eraFk5hz iauFk5hz
#  define eraGd2gc iauGd2gc
#  define eraGmst06 iauGmst06
#  define eraHfk5z iauHfk5z
#  define eraIr iauIr
#  define eraJd2cal iauJd2cal
#  define eraNut06a iauNut06a
#  define eraObl06 iauObl06
#  define eraP06e iauP06e
#  define eraPap iauPap
#  define eraPas iauPas
#  define eraPdp iauPdp
#  define eraPlan94 iauPlan94
#  define eraPmat06 iauPmat06
#  define eraPn iauPn
#  define eraPnm06a iauPnm06a
#  define eraPxp iauPxp
#  define eraRefco iauRefco
#  define eraRm2v iauRm2v
#  define eraRv2m iauRv2m
#  define eraRx iauRx
#  define eraRxp iauRxp
#  define eraRxpv iauRxpv
#  define eraRxr iauRxr
#  define eraRy iauRy
#  define eraRz iauRz
#  define eraS2c iauS2c
#  define eraSepp iauSepp
#  define eraSeps iauSeps
#  define eraStarpm iauStarpm
#  define eraTf2a iauTf2a
#  define eraTf2d iauTf2d
#  define eraTr iauTr
#  define eraTrxp iauTrxp

/* These are from sofam.h */

#  define ERFA_WGS84 WGS84

#  define ERFA_DJ00 DJ00
#  define ERFA_DJY DJY
#  define ERFA_DAU DAU

# else

#  include "erfa.h"
#  include "erfam.h"

/* No further action required */

# endif

#endif
