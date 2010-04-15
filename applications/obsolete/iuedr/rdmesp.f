      SUBROUTINE RDMESP( TP, RECLEN, NREC, DATA, STATUS )
*+
*  Name:
*     SUBROUTINE RDMESP

*  Description:
*     Read IUE MELO or MEHI data array, and extract information.
*     Read the actual data array from VICAR file, then extract the
*     spectral information needed for storage in IUEDR.

*  History:
*     Jack Giddings      02-DEC-81     At4 version
*     Paul Rees          07-NOV-88     IUEDR Vn. 2.0
*     Martin Clayton     15-AUG-94     IUEDR Vn. 3.1-2

*  Method:
*     The first data line containing "Record Zero" is stored in the
*     common blocks CMUEZ1 and CMUEZ2. Only the new ground station
*     software contains CMUEZ2 data.
*     The basic contents of first and only order are then translated
*     and stored in the IUEDR Common Blocks.
*     The data are accessed from memory.

*-

*  Implicit:
      IMPLICIT NONE

*  Starlink includes:
      INCLUDE 'SAE_PAR'

*  Import:
      INTEGER TP                       ! Tape descriptor
      INTEGER RECLEN                   ! record size in short words
      INTEGER NREC                     ! number of records

      INTEGER*2 DATA( RECLEN, NREC )   ! data array

*  Export:
      INTEGER STATUS! status return

*  External references:
      LOGICAL STR_SIMLR                ! caseless string equality

*  Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMSAVE'
      INCLUDE 'CMSPC'
      INCLUDE 'CMSPEC'

*  Local variables:
      INTEGER IORDER                   ! order index
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Read data array.
      CALL VIC_RDAT( TP, 2, RECLEN, NREC, DATA, STATUS )
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Translate the Record Zero Contents.
      CALL VIC_MEZR( RECLEN, DATA, STATUS )
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Branch on type of data found.
      IF ( STR_SIMLR( RESOL, 'LORES\\') .AND. NORDER.EQ.1 ) THEN
         CALL VIC_MEOR( 1, RECLEN, NREC, DATA, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            NOSPEC = .FALSE.
            ORDERS( 1 ) = 1
         END IF

      ELSE IF ( STR_SIMLR( RESOL, 'HIRES\\' ) ) THEN

*     Process each echelle order.
         DO IORDER = 1, NORDER
            CALL VIC_MEOR( IORDER, RECLEN, NREC, DATA, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               RETURN
            END IF
         END DO
         NOSPEC = .FALSE.

      ELSE
         CALL ERROUT( 'Can only handle MELO or MEHI\\', STATUS )
      END IF

      END
