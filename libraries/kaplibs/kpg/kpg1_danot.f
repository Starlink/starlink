      SUBROUTINE KPG1_DANOT( NDF, COMP, DATLAB, STATUS )
*+
*  Name:
*     KPG1_DANOT

*  Purpose:
*     Generates an annotation from the NDF's label and units.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_DANOT( NDF, COMP, DATLAB, STATUS )

*  Description:
*     This routine examines the NDF for a label and units.  It creates a
*     string of the form "label (units)" or "label" if the label but not
*     the units are present.  If neither are present a string comprising
*     the array component followed by ' values in FILE' is created. Where
*     "FILE" is the NDF filename. If the label is defined but is blank,
*     this same default is used.  Blank units are omitted.  The units
*     are squared for the variance component.

*  Arguments:
*     NDF = INTEGER (Given)
*        The NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component: 'DATA', 'QUALITY', 'VARIANCE',
*        or 'ERROR', though it is used literally and not checked to
*        be a member of this set.
*     DATLAB = CHARACTER * ( * ) (Returned)
*        The composite annotation.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  The identifier should be associated with an NDF.

*  Copyright:
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 February 12 (MJC):
*        Original version.
*     1992 April 14 (MJC):
*        Bug arising when the label is defined but is blank was fixed.
*     2009 October 9 (TIMJ):
*        - Include NDF name in default label.
*        - Append component to label if not DATA
*     2009 November 13 (MJC):
*        Append component name of known length.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER
     :  NDF

      CHARACTER * ( * )
     :  COMP

*  Arguments Returned:
      CHARACTER * ( * )
     :  DATLAB

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER
     :  CHR_LEN                  ! Length of a character string less
                                 ! trailing blanks

*  Local Variables:
      LOGICAL                    ! True if:
     :  THERE                    ! The axis units are present

      CHARACTER * 256
     :  LABEL,                   ! NDF label
     :  NDFNAM,                  ! Name of NDF
     :  UNITS                    ! NDF units

      CHARACTER * 8
     :  CMPNAM                   ! Uppercase copy of the component

      INTEGER
     :  NCLAB,                   ! Number of characters in the label
     :  NCLABD,                  ! Number of characters in the default
                                 ! label
     :  NCUNIT,                  ! Number of characters in the units
     :  NMLEN                    ! Length of NDFNAM

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Compose the annotation.
*    =======================

*    Set the string to the default in case there is no label, whereupon
*    the default is used.  Define the length of the default label.

      CALL KPG1_NDFNM( NDF, NDFNAM, NMLEN, STATUS )
      NCLABD = CHR_LEN( COMP )
      LABEL = COMP( :NCLABD )
      CALL CHR_APPND( ' values in', LABEL, NCLABD )
      CALL CHR_APPND( ' '//NDFNAM( : NMLEN), LABEL, NCLABD )

*    See if there is a label component present.

      CALL NDF_STATE( NDF, 'Label', THERE, STATUS )
      IF ( THERE ) THEN

*       Get the value and length in characters of the label.

         CALL NDF_CGET( NDF, 'Label', LABEL, STATUS )
         CALL NDF_CLEN( NDF, 'Label', NCLAB, STATUS )
         NCLAB = CHR_LEN( LABEL( :NCLAB ) )

*       Check for a blank or null string.  Use the default in this case.

         IF ( NCLAB .LT. 1 .OR. LABEL( 1:1 ) .EQ. CHAR( 0 ) ) THEN
            NCLABD = CHR_LEN( COMP )
            LABEL = COMP( :NCLABD )
            CALL CHR_APPND( ' values in', LABEL, NCLABD )
            CALL CHR_APPND( ' '//NDFNAM( : NMLEN), LABEL, NCLABD )
            NCLAB = NCLABD
         ELSE
*        For non-DATA case append the component
            CMPNAM = COMP
            CALL CHR_UCASE( CMPNAM )
            CALL CHR_LDBLK( CMPNAM )
            IF ( CMPNAM(:2) .NE. 'DA') THEN
               CMPNAM = COMP
               CALL CHR_APPND( ' '//CMPNAM, LABEL, NCLAB )
            END IF

         END IF
      ELSE
         NCLAB = NCLABD
      END IF

*    See if there is a units component present.

      CALL NDF_STATE( NDF, 'Units', THERE, STATUS )
      IF ( THERE .AND. COMP .NE. 'QUALITY' ) THEN

*       Obtain the units, allowing for the squared exponent when
*       dealing with variance, and its length in characters.

         CALL KPG1_DAUNI( NDF, COMP, UNITS, NCUNIT, STATUS )

*       Form the annotation, comprising the label and the units,
*       provided that the units are not blank.  Blank units are
*       omitted from the annotation.

         IF ( NCUNIT .LT. 1 .OR. UNITS( 1:1 ) .EQ. CHAR( 0 ) ) THEN
            DATLAB = LABEL( :NCLAB )
         ELSE
            DATLAB = LABEL( :NCLAB )//' ('//UNITS( :NCUNIT )//')'
         END IF
      ELSE

*        The annotation is just the label.

         DATLAB = LABEL( :NCLAB )
      END IF

      END
