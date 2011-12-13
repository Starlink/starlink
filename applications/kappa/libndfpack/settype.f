      SUBROUTINE SETTYPE( STATUS )
*+
*  Name:
*     SETTYPE

*  Purpose:
*     Sets a new numeric type for the data and variance components of
*     an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SETTYPE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application allows the numeric type of the data and variance
*     components of an NDF to be changed.  The NDF is accessed in update
*     mode and the values stored in these components are converted
*     in situ to the new type.  No other attributes of the NDF are
*     changed.

*  Usage:
*     settype ndf type

*  ADAM Parameters:
*     COMPLEX = _LOGICAL (Read)
*        If a TRUE value is given for this parameter, then the NDF's
*        array components will be altered so that they hold complex
*        values, an imaginary part containing zeros being created if
*        necessary.  If a FALSE value is given, then the components will
*        be altered so that they hold non-complex values, any imaginary
*        part being deleted if necessary.  If a null (!) value is supplied,
*        the value used is chosen so that no change is made to the current
*        state. [!]
*     DATA = _LOGICAL (Read)
*        If a TRUE value is given for this parameter, then the numeric
*        type of the NDF's data array will be changed.  Otherwise, this
*        component's type will remain unchanged. [TRUE]
*     NDF = NDF (Read and Write)
*        The NDF data structure whose array components are to have
*        their numeric type changed.
*     TYPE = LITERAL (Read)
*        The new numeric type to which the NDF's array components are
*        to be converted.  The value given should be one of the
*        following: _DOUBLE, _REAL, _INTEGER, _WORD, _UWORD, _BYTE or
*        _UBYTE (note the leading underscore).  Existing pixel values
*        stored in the NDF will not be lost, but will be converted to
*        the new type.  Any values which cannot be represented using the
*        new type will be replaced with the bad-pixel value.
*     VARIANCE = _LOGICAL (Read)
*        If a TRUE value is given for this parameter, then the numeric
*        type of the NDF's variance array will be changed.  Otherwise,
*        this component's type will remain unchanged. [TRUE]

*  Examples:
*     settype rawdata _real
*        Converts the data and variance values held in the NDF data
*        structure rawdata to have a numeric type of _REAL (i.e. to be
*        stored as single-precision floating-point numbers).
*     settype inst.run1 _word novariance
*        Converts the data array in the NDF structure inst.run1 to be
*        stored as word (i.e. Fortran INTEGER*2) values.  No change is
*        made to the variance component.
*     settype hd26571 _double complex
*        Causes the data and variance components of the NDF structure
*        hd26571 to be altered so as to hold complex values using
*        double precision numbers.  The existing pixel values are
*        converted to this new type.

*  Related Applications:
*     Figaro: RETYPE.

*  Timing:
*     The execution time is approximately proportional to the number of
*     pixel values to be converted.

*  Copyright:
*     Copyright (C) 1990, 1992 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-AUG-1990 (RFWS):
*        Original version.
*     25-SEP-1990 (RFWS):
*        Completed initial implementation.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1995 April 24 (MJC):
*        Made usage and examples lowercase.  Added Related Applications.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXNTYP             ! Number of possible numeric types
      PARAMETER ( MXNTYP = 7 )

*  Local Variables:
      CHARACTER * ( 14 ) COMP    ! Component list
      CHARACTER * ( MXNTYP * ( NDF__SZTYP + 1 ) - 1 ) TLIST ! Type list
      CHARACTER * ( NDF__SZFTP ) FTYPE ! Full type specification
      CHARACTER * ( NDF__SZTYP ) NTYPE( MXNTYP ) ! Numeric type strings
      CHARACTER * ( NDF__SZTYP ) TYPE ! New numeric type
      INTEGER IT                 ! Loop counter for type strings
      INTEGER NC                 ! No. characters in component list
      INTEGER NDF                ! NDF identifier
      LOGICAL CMPLX              ! NDF holds complex values?
      LOGICAL DATA               ! Set type of data component?
      LOGICAL VAR                ! Set type of variance component?

*  Local Data:
      DATA NTYPE / '_DOUBLE', '_REAL', '_INTEGER', '_WORD', '_UWORD',
     :             '_BYTE', '_UBYTE' /

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an NDF identifier.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', NDF, STATUS )

*  Obtain the NDF's current numeric array type as the default.
      CALL NDF_TYPE( NDF, 'Data,Variance', TYPE, STATUS )

*  See if the NDF holds complex values.
      CALL NDF_CMPLX( NDF, 'Data,Variance', CMPLX, STATUS )

*  Construct a list of possible new numeric types, putting the current
*  value at the start to act as a default.
      NC = 0
      CALL CHR_PUTC( TYPE, TLIST, NC )
      DO 1 IT = 1, MXNTYP
         IF ( NTYPE( IT ) .NE. TYPE ) CALL CHR_PUTC( ',' // NTYPE( IT ),
     :                                               TLIST, NC )
 1    CONTINUE
      CALL CHR_RMBLK( TLIST )

*  Obtain a new numeric type using the existing one as the default.
      CALL PAR_CHOIC( 'TYPE', 'NULL', TLIST, .TRUE., TYPE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  See if the NDF is to hold complex values.  Suggest the current
*  complex value flag as the default, and use this value if a null value
*  is supplied.
      CALL PAR_DEF0L( 'COMPLEX', CMPLX, STATUS )
      CALL PAR_GET0L( 'COMPLEX', CMPLX, STATUS )
      IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  Determine if the data component's type is to be set.
      CALL PAR_GET0L( 'DATA', DATA, STATUS )

*  See if a variance component is present.  If so, then determine if
*  it's type is to be set.
      CALL NDF_STATE( NDF, 'Variance', VAR, STATUS )
      IF ( VAR ) CALL PAR_GET0L( 'VARIANCE', VAR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Generate a character string containing the list of selected
*  components.
      NC = 0
      IF ( DATA ) CALL CHR_PUTC( 'Data,', COMP, NC )
      IF ( VAR ) CALL CHR_PUTC( 'Variance,', COMP, NC )
      NC = NC - 1

*  If at least one component was selected, then generate the new full
*  array type specification.
      IF ( DATA .OR. VAR ) THEN
         FTYPE = TYPE
         IF ( CMPLX ) FTYPE = 'COMPLEX' // TYPE

*  Set the new type.
         CALL NDF_STYPE( FTYPE, NDF, COMP( : NC ), STATUS )
      END IF

*  End the NDF context.
 99   CONTINUE
      CALL NDF_END( STATUS )

*  If an error occurred, then report contextual information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SETTYPE_ERR',
     :     'SETTYPE: Error setting a new data type for an NDF''s ' //
     :     'array components.', STATUS )
      END IF

      END
