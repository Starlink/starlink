      SUBROUTINE sgs_1ERR (NERR, RNAME, MES, JSTAT)
*+
*  Name:
*     ERR

*  Purpose:
*     Report SGS error.

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     internal routine

*  Arguments:
*     NERR = INTEGER (Given)
*         SGS internal error number
*     RNAME = CHAR (Given)
*         Name of routine reporting error
*     MES = CHAR (Given)
*         Error message
*     JSTAT = INTEGER (Returned)
*         SGS error status

*  Notes:
*     The SGS internal error number and the SGS error status are the
*     same in this implementation.
*
*     The entire message (text plus routine name etc.) will be
*     truncated to 72 characters.

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

*  Externals:
*     ems_REP

*-

      IMPLICIT NONE

      INTEGER NERR
      CHARACTER*(*) RNAME,MES
      INTEGER JSTAT

      CHARACTER LINE*72, ID*10



      JSTAT = NERR
      LINE =  'SGS_'//RNAME//' - '//MES
      ID = 'SGS__'//RNAME
      CALL EMS_REP(ID, LINE, JSTAT)

      END
