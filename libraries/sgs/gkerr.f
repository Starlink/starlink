      SUBROUTINE sgs_1GKERR (RNAME, JSTAT)
*+
*  Name:
*     GKERR

*  Purpose:
*     Test for GKS error.

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     Internal routine

*  Description:
*     If the status is bad on entry, it is returned unchanged.  If
*     not, and if there has been a GKS error since this routine
*     was last called, an error value is returned and an error
*     reported.
*
*     This routine will only detect a GKS error if it is the last
*     error reported and the GKS error has not been flushed - i.e. only
*     GKS errors that occured after the error stack was marked can be
*     detected.
*
*     It will only work if the Starlink GKS error handler which reports
*     errors via EMS is present.

*  Arguments:
*     RNAME = CHAR (Given)
*         Name of calling routine
*     JSTAT = INTEGER (Given & Returned)
*         Status

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     07-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Errors:
*     GKS error

*  Externals:
*     sgs_1ERR, ems_STAT

*-

      IMPLICIT NONE

      CHARACTER*(*) RNAME
      INTEGER JSTAT

      INCLUDE 'SGS_ERR'

      INCLUDE 'GKS_ERR'


      INTEGER LASTER



      IF (JSTAT.EQ.0) THEN

*     Test error stack for the presence of an error
         CALL ems_STAT(LASTER)
         IF ( LASTER.EQ.GKS__ERROR)
     :      CALL sgs_1ERR(SGS__GKSER,RNAME,'GKS error detected',JSTAT)
      END IF

      END
