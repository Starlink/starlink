      SUBROUTINE PREPA4( NCARD, FITS, TYPE, STATUS )
*+
*  Name:
*     PREPA4

*  Purpose:
*     Get type of input image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPA4( NCARD, FITS, TYPE, STATUS )

*  Description:
*     The FITS keywords are examined to determine the source of the
*     image. The keyword VERSION is examined to determine if the image
*     was produced by the YORIC (or HIRES) processor at IPAC. If not,
*     the keyword INSTRUME is examined. If it has a recognised value,
*     that value is returned in TYPE. If not, the value of the constant
*     IRI__NONAM is returned to indicate that the image is of an unknwon
*     type.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of FITS header cards.
*     FITS( NCARD ) = CHARACTER * ( * ) (Given)
*        The FITS header cards.
*     TYPE = CHARACTER * ( * ) (Returned)
*        The image type. This is returned equal to one of the symbolic
*        constants defined within the IRI subsystem.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     HM: Huw Morris (IPMAF)
*     {enter_new_authors_here}

*  History:
*     14-DEC-1992 (DSB):
*        Original version.
*     29-NOV-1994 (HM):
*        Corrected to recognise second release ISSA plates
*        Corrected in case of not recognising INSTRUME
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'IRI_PAR'          ! IRI_ constants

*  Arguments Given:
      INTEGER NCARD
      CHARACTER FITS( NCARD )*(*)

*  Arguments Returned:
      CHARACTER TYPE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! True if 2 strings are equal apart
                                 ! from case.

*  Local Variables:
      CHARACTER CMNT*72          ! Comment string from a FITS keyword.

      INTEGER CARD               ! Card number at which the required
                                 ! FITS keyword was found.
      LOGICAL THERE              ! True if the required FITS keyword
                                 ! exists.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the type to be unknown.
      TYPE = IRI__NONAM

*  Attempt to get the comment associated with the keyword VERSION.
      CALL IRM_GKCMT( NCARD, FITS, 1, 'VERSION', '/', 0, THERE, CMNT,
     :                CARD, STATUS )

*  If the keyword VERSION exists ...
      IF( THERE ) THEN

*  And the comment states the version of YORIC, the image is a YORIC
*  image.
         CALL CHR_LDBLK( CMNT )
         CALL CHR_UCASE( CMNT )
         IF( CHR_SIMLR( CMNT, 'VERSION OF YORIC' ) ) TYPE = IRI__YORIC

      END IF

*  If the image type has not yet been determined...
      IF( TYPE .EQ. IRI__NONAM ) THEN

*  ...get the value of FITS keyword INSTRUME.
         CALL IRM_GKEYC( NCARD, FITS, 1, 'INSTRUME', THERE, TYPE, CARD,
     :                STATUS )

*  Remove blanks and convert it to upper case.
         CALL CHR_RMBLK( TYPE )
         CALL CHR_UCASE( TYPE )

*  Check for one of the release II ISSA types, and if present, flag as
*  a generic ISSA type
         IF (TYPE .EQ. IRI__ISI .OR.
     :       TYPE .EQ. IRI__ISII .OR.
     :       TYPE .EQ. IRI__ISFD .OR.
     :       TYPE .EQ. IRI__ISRJ ) TYPE = IRI__ISSA

*  Check that the type is one of the recognised types. Otherwise, use 
*  IRI__NONAM.
         IF( TYPE .NE. IRI__CPC .AND.      
     :       TYPE .NE. IRI__SKYFL .AND.
     :       TYPE .NE. IRI__GALPL .AND.
     :       TYPE .NE. IRI__ALLSK .AND.
     :       TYPE .NE. IRI__DSCO .AND.
     :       TYPE .NE. IRI__ISSA .AND.
     :       TYPE .NE. IRI__YORIC .AND.
     :       TYPE .NE. IRI__MAPCR .AND.
     :       TYPE .NE. IRI__NONAM ) THEN

            CALL MSG_BLANKIF( MSG__QUIET, STATUS )
            CALL MSG_SETC( 'T1', TYPE )
            CALL MSG_SETC( 'T2', IRI__NONAM )
            CALL MSG_OUTIF( MSG__QUIET, 'PREPA4_MSG1',
     :   '    Unknown value "("^T1")" found for FITS keyword INSTRUME.',
     :                      STATUS )
            TYPE = IRI__NONAM

         END IF      

      END IF

      END
