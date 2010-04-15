      SUBROUTINE PREPB3( NCARD, FITS, LOC, STATUS )
*+
*  Name:
*     PREPB3

*  Purpose:
*     Create and write extra components to IMAGE_INFO of a SKYFLUX image

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPB3( NCARD, FITS, LOC, STATUS )

*  Description:
*     This routine create following components in the IMAGE_INFO
*     structure of a SKYFLUX NDF.
*            HCON <_INTEGER>
*            MINSOP <_INTEGER>
*            MAXSOP <_INTEGER>
*            SKYFLUX <_INTEGER>
*            SKYWEIGHT <_LOGICAL>
*     Where
*        HCON - the hours confirmation of the SKYFLUX image.
*        MINSOP - the minimum SOP number.
*        MAXSOP - the maximum SOP number.
*        SKYFLUX - the plate number of the SKYFLUX image.
*        SKYWEIGHT - If it is true, the image is a SKYFLUX weight
*                    image, otherwise, it is the SKYFLUX  Intensity
*                    image.
*
*     The components will be assigned values according to the
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
*     29-AUG-1991 (WG):
*        Original version.
*     3-DEC-1992 (DSB):
*        Name changed from SKINFO to PREPB3, etc.
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
      LOGICAL CHR_SIMLR          ! True if 2 strings are equal apart
                                 ! from case.

*  Arguments Given:
      INTEGER NCARD
      CHARACTER FITS( NCARD )*(*)
      CHARACTER LOC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BUNIT*5          ! Value of keyword BUNIT
      CHARACTER CMNT*72          ! A comment string from FITS head
      CHARACTER HCNSTR*1         ! Hour confirmation string
      CHARACTER MAXSTR*4         ! String expression of max. sop number
      CHARACTER MINSTR*4         ! String expression of min. sop number
      CHARACTER OBJECT*20        ! Value of keyword OBJECT
      CHARACTER PLTSTR*3         ! Plate number string


      INTEGER CARD               ! Card number of a FITS keyword
      INTEGER CMNTLN             ! Used length of CMNT
      INTEGER FEQPSN             ! Position of the fisrt equal sign
      INTEGER HCNPSN             ! Position of hour confirmation string
      INTEGER HCON               ! Hours confirmation of the image
      INTEGER MAXSOP             ! Maximum SOP number
      INTEGER MINSOP             ! Minimum SOP number
      INTEGER PLTNUM             ! SKYFLUX plate number
      INTEGER PLTPSN             ! Position of plate number string
      INTEGER SEPPSN             ! Position of the separation sign
      INTEGER SEQPSN             ! Position of the second equal sign
      INTEGER STCARD             ! Start card number when search FITS


      LOGICAL INSTY              ! Intensity image flag
      LOGICAL THERE              ! FITS keyword flag
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create required components within IMAGE_INFO structure.
      CALL DAT_NEW0I( LOC, 'HCON', STATUS )
      CALL DAT_NEW0I( LOC, 'MINSOP', STATUS )
      CALL DAT_NEW0I( LOC, 'MAXSOP', STATUS )
      CALL DAT_NEW0I( LOC, 'SKYFLUX', STATUS )
      CALL DAT_NEW0L( LOC, 'SKYWEIGHT', STATUS )

*  Get the value of FITS keyword OBJECT.
      CALL IRM_GKEYC( NCARD, FITS, 1, 'OBJECT', THERE, OBJECT, CARD,
     :             STATUS )

*  Remove all blanks in the string.
      CALL CHR_RMBLK( OBJECT )

*  Find the positions of 'PL' and 'H' in OBJECT string.
      PLTPSN = INDEX( OBJECT, 'PL' )
      HCNPSN = INDEX( OBJECT, 'H' )

*  Extract the plate number string and the hours confirmation string
*  from the OBJECT string.
      PLTSTR = OBJECT( PLTPSN + 2 : HCNPSN - 1 )
      HCNSTR = OBJECT( HCNPSN + 1 : )

*  Get the plate number and the hours confirmation from their string
*  expression.
      CALL CHR_CTOI( PLTSTR, PLTNUM, STATUS )
      CALL CHR_CTOI( HCNSTR, HCON, STATUS )

*  Write the SKYFLUX and HCON components of the IMAGE_INFO structure.
      CALL CMP_PUT0I( LOC, 'SKYFLUX', PLTNUM, STATUS )
      CALL CMP_PUT0I( LOC, 'HCON', HCON, STATUS )

*  Get the first FITS comment string after keyword OBJECT.
      STCARD = CARD + 1
      CALL IRM_COMNT( NCARD, FITS, STCARD, THERE, CMNT, CARD, STATUS )

*  Remove all blanks and get its used length.
      CALL CHR_RMBLK( CMNT )
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

*  Get value of FITS keyword BUNIT.
      CALL IRM_GKEYC( NCARD, FITS, 1, 'BUNIT', THERE, BUNIT, CARD,
     :             STATUS )

*  If the unit is 'JY/SR', the image is SKYFLUX Intensity.
      IF ( CHR_SIMLR( BUNIT, 'JY/SR' ) ) THEN
         INSTY = .TRUE.

*  Otherwise it is Image Weight.
      ELSE
         INSTY = .FALSE.
      END IF

*  Write the SKYWEIGHT component.
      CALL CMP_PUT0L( LOC, 'SKYWEIGHT', .NOT.INSTY, STATUS )

      END
