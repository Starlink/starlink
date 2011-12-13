#include "config.h"

*+
*  Name:
*     NUM_CMN

*  Purpose:
*     NUM_ common block definitions for numerical error trapping

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
*     Copyright (C) 1997, 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     R.F. Warren-Smith (DUVAD::RFWS)
*     BLY: M.J. Bly  (Starlink, RAL)
*     NXG: Norman Gray (Starlink, Glasgow)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     16-AUG-1988 (RFWS):
*         Original version
*     22-JAN-1997 (BLY):
*         System specific versions for Unix (BLY)
*     21-MAR-2004 (NXG):
*         Autoconf (NXG)
*     15-AUG-2005 (TIMJ):
*         Should not mark a COMMON as VOLATILE

*-

*   Exception status variable to indicate a numerical error has been
*   detected.
      INTEGER NUM_ERROR


*   Common block.
      COMMON /NUM_CMN/ NUM_ERROR


*   Save the common block contents.
      SAVE /NUM_CMN/


*   Prevent compiler optimisation of common variables accessed by a
*   condition/signal handler.  The VOLATILE keyword is present in
*   Fortran 2000, and as a vendor extension in some earlier compilers.
*   Its purpose is to indicate that a given variable may change other
*   than under the control of the program, or as a result of external
*   events -- it's for device handling.  I (Norman Gray/nxg, autoconfing
*   this) do not see that this is relevant for a signal handler, and
*   very strongly suspect that this is voodoo.  It has been added in the
*   past, however (by AJC in 24-NOV-2002) so presumably there was some
*   reason for it.
#if HAVE_KEYWORD_VOLATILE
*   It is non portable marking the COMMON block as VOLATILE as well as
*   the contents. Now simply mark the content as VOLATILE.
      VOLATILE NUM_ERROR
#else

* This platform doesn't have VOLATILE
#endif



************************************************************************
*				END				       *
*                                                                      *
*      NUM_ common block definitions for numerical error trapping      *
*                                                                      *
*				END                                    *
************************************************************************
