      SUBROUTINE PREPB5( NCARD, FITS, LOC, STATUS )
*+
*  Name:
*     PREPB5

*  Purpose:
*     Create and write extra components in IMAGE_INFO of an ALLSKY NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPB5( NCARD, FITS, LOC, STATUS )

*  Description:
*     This routine creates following components in the IMAGE_INFO
*     structure of an IRAS ALLSKY NDF file.
*                     HCON <_INTEGER>
*                     MINSOP <_INTEGER>
*                     MAXSOP <_INTEGER>
*                     GALCEN <_LOGICAL>
*     Where
*        HCON - the hour confirmation of the All Sky Image.
*        MINSOP - the minimum SOP number.
*        MAXSOP - the maximun SOP number.
*        GALCEN - if it is true, the AllSky image is centred on the
*                 Galactic Center, otherwise it is centred on the
*                 Galactic Anti-Center.
*
*     These components will be assigned the values according to the
*     information obtained from the FITS extension of the NDF.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of card images in the FITS extension of the NDF.
*     FITS( NCARD ) = CHARACTER*( * ) (Given)
*        The FITS header cards.
*     LOC = CHARACTER*( * ) (Given)
*        The locator of the IMAGE_INFO structure of the NDF file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-SEP-1991 (WG):
*        Original version.
*     3-DEC-1992 (DSB):
*        Name changed from ASINFO to PREPB5, etc.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Arguments Given:
      INTEGER NCARD
      CHARACTER FITS( NCARD )*(*)
      CHARACTER LOC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CMNT*72          ! A comment string from FITS head
      CHARACTER HCNSTR*1         ! String expressing of HCON
      CHARACTER MAXSTR*4         ! String expression of max. sop number
      CHARACTER MINSTR*4         ! String expression of min. sop number
      CHARACTER OBJECT*10        ! Value of keyword OBJECT


      INTEGER CARD               ! Card number of a FITS keyword
      INTEGER CMNTLN             ! Used length of CMNT
      INTEGER FEQPSN             ! Position of the fisrt equal sign
      INTEGER HCON               ! Hours confirmation of the image
      INTEGER MAXSOP             ! Value of keyword MAXSOP
      INTEGER MINSOP             ! Value of keyword MINSOP
      INTEGER OBJLN              ! Length of string OBJECT
      INTEGER SEPPSN             ! Position of the separation sign
      INTEGER SEQPSN             ! Position of the second equal sign
      INTEGER STCARD             ! Start card number when search FITS
      LOGICAL CENTRE             ! Galactic centre image flag
      LOGICAL THERE              ! FITS keyword flag

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create required components within IMAGE_INFO structure.
      CALL DAT_NEW0I( LOC, 'HCON', STATUS )
      CALL DAT_NEW0I( LOC, 'MINSOP', STATUS )
      CALL DAT_NEW0I( LOC, 'MAXSOP', STATUS )
      CALL DAT_NEW0L( LOC, 'GALCEN', STATUS )

*  Get the value of FITS keyword OBJECT.
      CALL IRM_GKEYC( NCARD, FITS, 1, 'OBJECT', THERE, OBJECT, CARD,
     :             STATUS )

*  Remove the leading blanks and get its used length.
      CALL CHR_LDBLK( OBJECT )
      OBJLN = CHR_LEN( OBJECT )

*  Get the last character of the OBJECT string.
      HCNSTR = OBJECT( OBJLN : OBJLN )

*  Convert it to an integer.
      CALL CHR_CTOI( HCNSTR, HCON, STATUS )

*  Write the HCON components of the IMAGE_INFO structure.
      CALL CMP_PUT0I( LOC, 'HCON', HCON, STATUS )

*  If the first character of object string is 'C', the image is
*  Galactic Centre.
      IF ( OBJECT( 1 : 1 ) .EQ. 'C' ) THEN
         CENTRE = .TRUE.

*  Or if the first character of object string is 'A', the image is
*  Galactic Anti-Centre.
      ELSE IF ( OBJECT( 1 : 1 ) .EQ. 'A' ) THEN
         CENTRE = .FALSE.
      END IF

*  Write the component GELCEN.
      CALL CMP_PUT0L( LOC, 'GALCEN', CENTRE, STATUS )

*  Get the first FITS comment string after keyword OBJECT.
      STCARD = CARD
      CALL IRM_COMNT( NCARD, FITS, STCARD, THERE, CMNT, CARD, STATUS )

*  Remove the leading blank and get its used length.
      CALL CHR_LDBLK( CMNT )
      CMNTLN = CHR_LEN( CMNT )

*  Get the positions of first '=', the ';' and the second '='.
      FEQPSN = INDEX( CMNT, '=' )
      SEPPSN = INDEX( CMNT, ';' )
      SEQPSN = INDEX( CMNT( SEPPSN + 1 : ), '=' )
      SEQPSN = SEPPSN + SEQPSN

*  Extract the MINSOP and MAXSOP string expression for the CMNT string.
      MINSTR = CMNT( FEQPSN + 1 : SEPPSN -1 )
      MAXSTR = CMNT( SEQPSN + 1 : CMNTLN )

*  Covert them to integers.
      CALL CHR_CTOI( MINSTR, MINSOP, STATUS )
      CALL CHR_CTOI( MAXSTR, MAXSOP, STATUS )

*  Write them into MINSOP and MAXSOP components of the IMAGE_INFO
*  structure.
      CALL CMP_PUT0I( LOC, 'MINSOP', MINSOP, STATUS )
      CALL CMP_PUT0I( LOC, 'MAXSOP', MAXSOP, STATUS )

      END
