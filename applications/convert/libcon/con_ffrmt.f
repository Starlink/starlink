      SUBROUTINE CON_FFRMT( BITPIX, UNSIGN, BPV, FMTIN, STATUS )
*+
*  Name:
*     CON_FFRMT

*  Purpose:
*     Obtains the input HDS data format for a sequential FITS-like file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_FFRMT( BITPIX, UNSIGN, BPV, FMTIN, STATUS )

*  Description:
*     This is a server routine for ASCII2NDF and UNF2NDF.  It packages
*     up the operations required to define the input data format
*     (HDS types) from information given in a FITS-like header.

*  Arguments:
*     BITPIX = INTEGER (Given)
*        The value of the BITPIX keyword in the FITS header, i.e. the
*        number of bits per data value.  If it is negative this
*        indicates floating-point data following the FITS-like header.
*     UNSIGN = LOGICAL (Given)
*        If true the data are unsigned as given by the UNSIGNED being
*        true in the FITS header.
*     BPV = INTEGER (Returned)
*        The number of bytes per data value.
*     FMTIN = CHARACTER * ( * ) (Returned)
*        The HDS format of the data in the sequential file with the
*        FITS-like header.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 September 18 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER BITPIX
      LOGICAL UNSIGN

*  Arguments Returned:
      INTEGER BPV
      CHARACTER * ( * ) FMTIN

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Use BITPIX to determine the data format.
*  ========================================

*  Find the number of bytes per data value, note the absolute because
*  BITPIX can be negative.
      BPV = ABS( BITPIX / 8 )

*  Find the input format type.
      IF ( BITPIX .LT. 0 ) THEN

*  Firstly, assign the floating-point values.
         IF ( BPV .EQ. 4 ) THEN
            FMTIN = '_REAL'
         ELSE IF ( BPV .EQ. 8 ) THEN
            FMTIN = '_DOUBLE'
         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'INVTYP',
     :        'The FITS object has an invalid value for BITPIX.', 
     :        STATUS )
         END IF
      ELSE

*  Secondly, the integer types.
         IF ( BPV .EQ. 1 .AND. UNSIGN ) THEN
            FMTIN = '_UBYTE'
         ELSE IF ( BPV .EQ. 2 .AND. UNSIGN ) THEN
            FMTIN = '_UWORD'
         ELSE IF ( BPV .EQ. 1 ) THEN
            FMTIN = '_BYTE'
         ELSE IF ( BPV .EQ. 2 ) THEN
            FMTIN = '_WORD'
         ELSE IF ( BPV .EQ. 4 ) THEN
            FMTIN = '_INTEGER'
          ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'INVTYP',
     :        'The FITS object has an invalid value for BITPIX.', 
     :        STATUS )
         END IF
      END IF

      END

