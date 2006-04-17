      SUBROUTINE ERR_STAT( STATUS )
*+
*  Name:
*     ERR_STAT

*  Purpose:
*     Inquire the last reported error status.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERR_STAT( STATUS )

*  Description:
*     The current error context is checked for any error messages pending 
*     output. If none exist, the status argument is returned set to 
*     SAI__OK. If any messages have been reported, the status argument is 
*     returned set to the last reported value.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The global status: it returned set to the last reported
*        error status within the current error context; if none exist,
*        it is returned set to SAI__OK.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-SEP-1990 (PCTR):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Status:
      INTEGER STATUS

*.

*  Call EMS_STAT.
      CALL EMS_STAT( STATUS )

      END
