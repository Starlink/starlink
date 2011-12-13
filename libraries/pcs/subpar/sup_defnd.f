      SUBROUTINE SUBPAR_DEFND ( NAMECODE, NDIM, MAXD, VALUES, ACTD,
     :  STATUS )
*+
*  Name:
*     SUBPAR_DEFND

*  Purpose:
*     Set dynamic default values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_DEFND ( NAMECODE, NDIM, MAXD, VALUES, ACTD,

*  Description:
*     Set default values for an n-dimensional primitive object associated
*     with a Parameter.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     to contain the object array.
*     There is a routine for each access type:

*        PAR_DEFNC    CHARACTER
*        PAR_DEFND    DOUBLE PRECISION
*        PAR_DEFNR    REAL
*        PAR_DEFNI    INTEGER
*        PAR_DEFNL    LOGICAL

*     If the object data type differs from the access type, then
*     conversion is performed (if allowed).

*  Arguments:
*     NAMECODE=INTEGER (given)
*        Code-number of the parameter
*     NDIM=INTEGER (given)
*        Expression specifying the number of dimensions of the
*        program array, VALUES.   This must match the actual number
*        of object dimensions.
*     MAXD(NDIM)=INTEGER (given)
*        Array specifying the dimensions of the program array
*        containing the data values.
*     VALUES(*)=DOUBLE PRECISION (given)
*        Array containing the default values.
*     ACTD(NDIM)=INTEGER (given)
*        Array containing the actual data dimensions.   These must
*        match the actual object dimensions.
*     STATUS=INTEGER

*  Algorithm:
*     Get a locator to the space in the tasks private HDS parameter
*     store for storing the dynamic default for the parameter. Store the
*     values in HDS after doing any necessary type conversions.

*  Copyright:
*     Copyright (C) 1984, 1985, 1992, 1993 Science & Engineering Research Council.
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
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-DEC-1984 (BDK):
*        Original
*     05-SEP-1985 (BDK):
*        Check status before calling INCOPY
*     30-JAN-1992 (AJC):
*        Use HDS not INCOPY for data conversion
*     11-MAR-1992 (AJC):
*        Remove unused POINTER
*     20-JUL-1992 (AJC):
*        Move DATA statement
*     26-FEB-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! SAI Constants
      INCLUDE 'DAT_PAR'

      INCLUDE 'SUBPAR_PAR'


*  Arguments Given:
      INTEGER NAMECODE                  ! Parameter Name
      INTEGER NDIM			! Number of dimensions
      INTEGER MAXD(*)			! Array dimensions
      DOUBLE PRECISION VALUES(*)			! Array to supply values
      INTEGER ACTD(*)			! Object dimensions

*    Status return :
      INTEGER STATUS			! Status Return


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  Local Variables:
      CHARACTER*(DAT__SZLOC) BOTLOC     ! locator to HDS object
      CHARACTER*15 HDSTYPE              ! type of primitive data
                                        ! to be stored
      INTEGER TYPE                      ! encoded data type
      CHARACTER*15 POSTYPES(5)          ! possible primitive data types


*  Local Data:
      DATA POSTYPES / '_CHAR', '_REAL', '_DOUBLE', '_INTEGER',
     :  '_LOGICAL' /




*.


      IF (STATUS .NE. SAI__OK) RETURN

*
*   Get the type of the parameter
*
      TYPE = MOD ( PARTYPE(NAMECODE), 10 )
*
*   Get character version of type
*
      HDSTYPE = POSTYPES(TYPE)
      IF ( TYPE .EQ. SUBPAR__CHAR ) THEN
*
*      Invent an oversize value
*
         HDSTYPE = '_CHAR*132'
      ENDIF
*
*   Create the necessary space and return a locator to it
*   The object is created with the type of the parameter
*
      CALL SUBPAR_CREDYN ( NAMECODE, HDSTYPE, NDIM, ACTD, BOTLOC,
     :        STATUS )
*
*   Now put the values in the object, HDS converts if necessary
      CALL DAT_PUTND ( BOTLOC, NDIM, MAXD, VALUES, ACTD, STATUS )
*
*   Set the HDS name as the dynamic default
*
      CALL SUBPAR_DATDEF ( NAMECODE, BOTLOC, STATUS )
*
*   Annul the locator.
*
      CALL DAT_ANNUL ( BOTLOC, STATUS )

      END
