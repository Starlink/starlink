      SUBROUTINE VIC_RDAT( TP, NZ, NX, NY, Z, STATUS )
*+
*  Name:
*     SUBROUTINE VIC_RDAT

*  Purpose:
*     Read the data part of a VICAR image.

*  Description:
*     The VICAR image is read from tape as a series of blocks.
*     The blocks must be of a size that contains an integral number
*     of image lines.
*     For multi-byte pixels, byte reversal is performed here.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     DMILLS: Dave Mills (UCL)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     28-OCT-81 (JRG):
*       AT4 version.
*     26-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*     22-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Some restructuring and final conversion to SGP/16 style.
*     01-OCT-92 (DMILLS):
*       IUEDR Vn. 3.0
*       ADAMised version to run on multiple hardware platforms
*       Added device independence (MAGTAPE/DISK/NFS-DISK)
*     15-JUL-94 (MJC):
*       IUEDR Vn. 3.1-1
*       Tidy-up.
*     09-JUN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMTAPE'
      INCLUDE 'CMISAF'

*  Arguments Given:
      INTEGER TP             ! Tape descriptor.
      INTEGER NZ             ! Number of bytes per pixel.
      INTEGER NX             ! Number of pixels in x-direction.
      INTEGER NY             ! Number of pixels in y-direction.

*  Arguments Returned:
      BYTE Z( NZ, NX, NY )   ! Image array.

*  Status:
      INTEGER STATUS         ! Global status.

*  Local Constants:
      INTEGER MAXBUF         ! Maximum VICAR block size.
      PARAMETER ( MAXBUF = 6144 )

*  Local Variables:
      BYTE BUF( MAXBUF )     ! VICAR tape block.
      INTEGER ILINE          ! Line index in block.
      INTEGER IP             ! Loop index.
      INTEGER IX             ! Loop index.
      INTEGER IY             ! Loop index.
      INTEGER IZ             ! Byte index in pixel.
      INTEGER FDN
      INTEGER NLINE          ! Number of lines per block.
      INTEGER NREAD          ! Number of bytes read from tape.
      INTEGER ISTAT          ! Local status.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Read data blocks until the image is full
      IY = 0

      DO WHILE ( IY .LT. NY )
         CALL IUE_DEVICE( 'READ', TP, FDN, BUF, MAXBUF, NREAD, STATUS )
         IF ( NREAD .EQ. 2000 ) THEN
             NX = 2000

         ELSE IF ( NREAD .EQ. 1000 ) THEN
             NX = 1000
         END IF

         IF ( STATUS .NE. SAI__OK ) THEN
            IF ( .NOT. ISAFILE ) THEN
               CALL ERROUT( 'reading VICAR image\\', STATUS )
            END IF
            GO TO 100

         ELSE IF ( MOD( NREAD, NZ * NX ) .NE. 0 ) THEN
            CALL ERRSTR( 'VICAR block size \\' )
            CALL ERRINT( NREAD )
            CALL ERROUT( ' unexpected\\', STATUS )
            GO TO 100
         END IF

         NLINE = NREAD / ( NZ * NX )
         IP = 0

         DO ILINE = 1, NLINE
            IY = IY + 1
            DO IX = 1, NX
               DO IZ = 1, NZ
                  IP = IP + 1
                  Z( IZ, IX, IY ) = BUF( IP )
               END DO
            END DO
            CALL GEN_REV( NZ, NX, Z( 1, 1, IY ) )
         END DO
      END DO

*  Skip over tape mark at end.
      IF ( .NOT. ISAFILE ) THEN
         ISTAT = SAI__OK
         CALL MAG_SKIP( TCHAN, 1, ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL ERR_FLUSH( ISTAT )
            CALL ERR_OUT( '\\', STATUS )
         END IF
      END IF

 100  CONTINUE

      END
