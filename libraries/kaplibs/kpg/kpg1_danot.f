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
*     the array component followed by ' values' is created.  If the
*     label is defined but is blank, this same default is used.  Blank
*     units are omitted.  The units are squared for the variance
*     component.

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

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 February 12 (MJC):
*        Original version.
*     1992 April 14 (MJC):
*        Bug arising when the label is defined but is blank was fixed.
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
     :  UNITS                    ! NDF units

      INTEGER
     :  NCLAB,                   ! Number of characters in the label
     :  NCLABD,                  ! Number of characters in the default
                                 ! label
     :  NCUNIT                   ! Number of characters in the units

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Compose the annotation.
*    =======================

*    Set the string to the default in case there is no label, whereupon 
*    the default is used.  Define the length of the default label.

      NCLABD = CHR_LEN( COMP )
      LABEL = COMP( :NCLABD )//' values'
      NCLABD = NCLABD + 7

*    See if there is a label component present.

      CALL NDF_STATE( NDF, 'Label', THERE, STATUS )
      IF ( THERE ) THEN

*       Get the value and length in characters of the label.

         CALL NDF_CGET( NDF, 'Label', LABEL, STATUS )
         CALL NDF_CLEN( NDF, 'Label', NCLAB, STATUS )
         NCLAB = CHR_LEN( LABEL( :NCLAB ) )

*       Check for a blank or null string.  Use the default in this case.

         IF ( NCLAB .LT. 1 .OR. LABEL( 1:1 ) .EQ. CHAR( 0 ) ) THEN
            LABEL = COMP( :NCLABD-7 )//' values'
            NCLAB = NCLABD
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
