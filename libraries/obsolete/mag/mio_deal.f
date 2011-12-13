
      SUBROUTINE MIO_DEAL(DEVICE, STATUS)
*+
*  Name:
*     MIO_DEAL

*  Purpose:
*     De-allocate tape device - a dummy routine in Unix.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MIO_DEAL(DEVICE, STATUS)

*  Description:
*     De-allocate a tape drive from continued use.

*  Arguments:
*     DEVICE=CHARACTER*(*) (Given)
*        A character string containing the name of the tape to be de-allocated.
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.

*  Copyright:
*     Copyright (C) 1983, 1991, 1992, 1993 Science & Engineering Research Council.
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
*     Sid Wright (UCL::SLW)
*     {enter_new_authors_here}

*  History:
*     15-Jul-1983:  Original.  (UCL::SLW)
*     15-Nov-1991:  Changed to new style prologue (RAL::KFH)
*           Replaced tabs by spaces in end-of-line comments (RAL::KFH)
*           Changed any fac_$name into fac1_name (RAL::KFH)
*           Inserted IMPLICIT NONE (RAL::KFH)
*     15-Jan-1992:  Made into a dummy for Unix. (RAL::KFH)
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     {enter_further_changes_here}

*  Notes:
*     This is the Unix version.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      CHARACTER*(*) DEVICE      ! Name of tape drive

*  Arguments Returned:
*    Status return :
      INTEGER STATUS            ! status return

*.

*   Literally does nothing at all.

      END
