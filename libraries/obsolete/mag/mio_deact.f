
      SUBROUTINE MIO_DEACT(STATUS)
*+
*  Name:
*     MIO_DEACT

*  Purpose:
*     terminate MIO system.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MIO_DEACT(STATUS)

*  Description:
*     The MIO system is run down.

*  Arguments:
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If the routine fails to
*        complete, this variable will be set to an appropriate error number.
*        If this variable is not SAI__OK on inpu, then the routine will
*        still attempt to execute, but will return the STATUS set to the
*        import value.

*  Algorithm:
*     The MIO_STOP routine is called to shut down MIO.

*  Copyright:
*     Copyright (C) 1983, 1991 Science & Engineering Research Council.
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
*     Sid Wright  (UCL::SLW)
*     {enter_new_authors_here}

*  History:
*     17-Apr-1983:  Starlink Version. (UCL::SLW)
*     15-Nov-1991:  Changed to new style prologue (RAL::KFH)
*           Replaced tabs by spaces in end-of-line comments (RAL::KFH)
*           Changed any fac_$name into fac1_name (RAL::KFH)
*           Inserted IMPLICIT NONE (RAL::KFH)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
*    Status return :
      INTEGER STATUS            ! status return

*  Local Variables:
      INTEGER ISTAT             ! Local status

*.


      ISTAT = STATUS
      STATUS = SAI__OK

*    Shut down MIO
      CALL MIO_STOP(STATUS)

      IF ( ISTAT.NE.SAI__OK ) STATUS = ISTAT

C      print *,'mio_deact:  status ', status
      RETURN
      END
