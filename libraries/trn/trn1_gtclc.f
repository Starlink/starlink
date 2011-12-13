      SUBROUTINE TRN1_GTCLC( SLOT, CTTC, CLASS, STATUS )
*+
*  Name:
*     TRN1_GTCLC

*  Purpose:
*     get classification array from the CTT.

*  Language:
*     Starlink Fortran

*  Invocation:
*     TRN1_GTCLC( SLOT, CTTC, CLASS, STATUS )

*  Description:
*     The routine obtains a logical classification array from the
*     requested slot in the compiled transformation table (CTT).  This
*     is not a user routine and the slot number supplied is not
*     validated before use.  TRN_GTCLC provides a user interface.

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     {enter_new_authors_here}

*  History:
*     9-MAY-1988:  Original version (DUVAD::RFWS)
*    16-NOV-1995:  Change LOGICAL array CLASS to BYTE (BKM)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_PAR'          ! TRN_ public constants


*  Global Variables:
      INCLUDE 'TRN_CMN'         ! TRN_ common blocks


*  Arguments Given:
      INTEGER SLOT              ! CTT slot to use
      INTEGER CTTC( TRN__MXCLS, TRN_SZCTT )
                                ! CTT classification array list


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
      BYTE CLASS( TRN__MXCLS )
                                ! Classification array


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      INTEGER ICLS              ! Loop counter for indexing
                                ! classification arrays


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Extract the required classification array from the CTT
*   classification array list.
      DO ICLS = 1, TRN__MXCLS
        CLASS( ICLS ) = CTTC( ICLS, SLOT )
      ENDDO


*   Exit routine.
      END
