      SUBROUTINE RDFIDS( FD, GEOMET, STATUS )
*+
*
*   Name:
*      SUBROUTINE RDFIDS
*
*   Description:
*      The contents of CMFIDS CMFIDT are read.
*
*   History:
*      Jack Giddings      01-MAY-82     AT4 version
*      Paul Rees          06-OCT-88     IUEDR Vn. 2.0
*      Martin Clayton     17-OCT-94     IUEDR Vn. 3.1-7
*         Fixed unformatted read fail by introducing FORMAT statements.
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Constants:
      INTEGER MAXFID              ! size of fiducial grid arrays
      INTEGER MAXLABEL            ! maximum label length
      INTEGER MAXNAME             ! maximum name length
      PARAMETER (MAXFID = 13, MAXLABEL = 40, MAXNAME = 16)

*   Import:
      INTEGER FD                  ! file descriptor

      LOGICAL GEOMET              ! whether geometric or not

*   Export:
      INTEGER STATUS              ! status return

*   External references:
      LOGICAL STR_SIMLR           ! caseless string equality

*   Global variables:
      INCLUDE 'CMFIDS'
      INCLUDE 'CMFIDT'

*   Local variables:
      INTEGER FY                  ! hmmm
      LOGICAL NODER               ! whether derivatives are defined

      CHARACTER*(MAXNAME) CTYPE   ! F77 type string
      CHARACTER*(MAXNAME) CID     ! F77 title string

      INTEGER I                   ! loop index
      INTEGER IX                  ! loop index
      INTEGER IY                  ! loop index
      INTEGER NCHAR               ! character count
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      NOFIDS = .TRUE.
      NOFIDT = .TRUE.

*   TYPE, LABEL
      READ ( FD, *, IOSTAT=STATUS ) CTYPE, CID
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading fiducial data type.\\', STATUS )
         RETURN
      END IF

      CALL GEN_CTOS( CTYPE, MAXNAME, FIDSTP, NCHAR )
      CALL STR_RMBLK( FIDSTP )
      CALL GEN_CTOS( CID, MAXLABEL, FIDSID, NCHAR )
      CALL STR_RMBLK( FIDSID )

*   Check type
      IF ( STR_SIMLR('FID_IUEG\\', FIDSTP) ) THEN
         IF ( .NOT. GEOMET ) THEN
            CALL ERROUT( 'Error: geometry type mis-match\\', STATUS )
            RETURN
         END IF

         NODER = .TRUE.

      ELSE IF ( STR_SIMLR('FID_IUER\\', FIDSTP)) THEN
         IF ( GEOMET ) THEN
            CALL ERROUT('Error: geometry mis-match\\', STATUS)
            RETURN
         END IF

         NODER = .TRUE.

      ELSE IF ( STR_SIMLR('FID_IUET\\', FIDSTP) ) THEN
         IF ( GEOMET ) THEN
            CALL ERROUT( 'Error: geometry type mis-match\\', STATUS )
            RETURN
         END IF

         NODER = .FALSE.

      ELSE
         CALL ERROUT( 'Error: invalid geometry type\\', STATUS )
         RETURN
      END IF

*   SIZE, THDA0, NFIDX, FIDX, NFIDY, FIDY
      READ ( FD, *, IOSTAT=STATUS ) FIDHW, FIDT0, NFIDX,
     :                              (FIDX(I), I = 1, MIN(NFIDX, 13)),
     :                              NFIDY,
     :                              (FIDY(I), I = 1, MIN(NFIDY, 13))

*   FIDFW changed to FIDHW here - IUEDR Vn. 2.0 (no other reference to
*   FIDFW in subroutine - probably a typo).
      IF ( FIDHW .LT. 0 ) THEN
         RETURN

      ELSE IF ( NFIDX.LT.2 .OR. NFIDX.GT.MAXFID ) THEN
         CALL ERRSTR( 'Error: number of x-fiducials \\' )
         CALL ERRINT( NFIDX )
         CALL ERROUT( ' illegal\\', STATUS )
         RETURN

      ELSE IF ( NFIDY.LT.2 .OR. NFIDY.GT.MAXFID ) THEN
         CALL ERRSTR( 'Error: number of y-fiducials \\' )
         CALL ERRINT( NFIDY )
         CALL ERROUT( ' illegal\\', STATUS )
         RETURN
      END IF

*   FIDS, FIDL, [FIDST, FIDLT]
      IF ( GEOMET ) THEN
         DO IY = 1, NFIDY
            DO IX = 1, NFIDX
               FIDS(IX, IY) = FIDX(IX)
               FIDL(IX, IY) = FIDY(IY)
               FIDQ(IX, IY) = 0
            END DO
         END DO

      ELSE
         DO IY = 1, NFIDY
            IF ( NODER ) THEN
               READ ( FD, *, IOSTAT=STATUS ) FY
               DO IX = 1, NFIDX
                  READ ( FD, 100, IOSTAT=STATUS )
     :                   FIDS(IX, IY), FIDL(IX, IY), FIDQ(IX, IY)
 100              FORMAT ( 5X, F6.2, F7.2, I5 )
               END DO

            ELSE
               READ ( FD, *, IOSTAT=STATUS ) FY
               DO IX = 1, NFIDX
                  READ ( FD, 200, IOSTAT=STATUS )
     :                  FIDS(IX, IY), FIDL(IX, IY), FIDST(IX, IY),
     :                  FIDLT(IX, IY), FIDQ(IX, IY)
 200              FORMAT ( 5X, F6.2, F7.2, F11.3, F7.3, I5 )
               END DO
            END IF
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERROUT( 'Error: reading fiducial data\\', STATUS )
               RETURN
            END IF

            DO IX = 1, NFIDX
               FIDS(IX, IY) = FIDS(IX, IY) + FIDX(IX)
               FIDL(IX, IY) = FIDL(IX, IY) + FIDY(IY)

               IF ( .NOT. NODER ) THEN
                  FIDS(IX, IY) = FIDS(IX, IY) + FIDT0 * FIDST(IX, IY)
                  FIDL(IX, IY) = FIDL(IX, IY) + FIDT0 * FIDLT(IX, IY)
               END IF
            END DO
         END DO
      END IF

      NOFIDS = .FALSE.
      NOFIDT = NODER

      END
