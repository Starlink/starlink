      SUBROUTINE PLIST( NAXIS1, NAXIS2, DATA, QUAL, NPRINT, STATUS )
*+
*  Name:
*     SUBROUTINE PLIST

*  Purpose:
*     The data quality in the stacked image is described.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PLIST( NAXIS1, NAXIS2, DATA, QUAL, NPRINT, STATUS )

*  Arguments:
*     NAXIS1 = INTEGER (Given)
*        Size of "Sample" Axis.
*     NAXIS2 = INTEGER (Given)
*        Size of "Line" Axis.
*     DATA = INTEGER*2 ( NAXIS1, NAXIS2 ) (Given)
*        The Image data.
*     QUAL = BYTE ( NAXIS1, NAXIS2 ) (Given)
*        Image Quality information.
*     NPRINT = INTEGER (Given)
*        Verbosity level for display.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*     Paul Rees          05-OCT-88     IUEDR Vn. 2.0
*     16-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER NAXIS1       ! size of axis 1 (sample)
      INTEGER NAXIS2       ! size of axis 2 (line)

      INTEGER*2 DATA( NAXIS1, NAXIS2 )   ! image

      BYTE QUAL( NAXIS1, NAXIS2 )        ! quality

      INTEGER NPRINT       ! print level

*  Status:
      INTEGER STATUS       ! Global status.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDATA'

*  Local Variables:
      CHARACTER TYPIX( 1200 )

      INTEGER SPIX( 1200 )
      INTEGER TNPIX( 1200 )
      INTEGER LPIX( 1200 )
      INTEGER QBITS( 8 )   ! individual data quality bits
      INTEGER DQ           ! workable data quality value
      INTEGER FN           ! FN value for pixel
      INTEGER I            ! loop index
      INTEGER IF           ! loop index
      INTEGER IFNMAX       ! maximum unsaturated FN
      INTEGER IL           ! loop index
      INTEGER IS           ! loop index
      INTEGER MAXFN
      INTEGER N            ! loop limit
      INTEGER NCHAR        ! character count
      INTEGER NPIX
      INTEGER NRES
      INTEGER NSAT
      INTEGER NTRN
      INTEGER NUSR
      INTEGER NXTR

      BYTE STR( 2 )        ! dummy string

*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Check against data.
      IF ( NODATA ) THEN
         CALL ERROUT( 'Error: no data\\', STATUS )
         RETURN
      END IF

*   Print.
      CALL LINE_WCONT( '%p%2w Image Data Quality:\\' )
      CALL PRTBUF( STATUS )

*   Initialise bad pixel counters.
      NPIX = 0
      NSAT = 0
      NTRN = 0
      NXTR = 0
      NUSR = 0
      NRES = 0
      MAXFN = 0

      DO IL = LMIN, LMAX
         DO IS = SMIN( IL ), SMAX( IL )
            IF ( DATA( IS, IL ) .EQ. DBLANK ) THEN

            ELSE IF ( QUAL( IS, IL ) .EQ. 0 ) THEN
               IF ( DATA( IS, IL ) .GT. MAXFN ) THEN
                  MAXFN = DATA( IS, IL )
               END IF

            ELSE
               CALL DQ_UTOI( QUAL( IS, IL ), DQ )
               CALL DQ_UNPK( DQ, 8, QBITS )
               IF ( QBITS( 7 ) .EQ. 0 ) THEN
                  IF ( DATA( IS, IL ) .GT. MAXFN ) THEN
                     MAXFN = DATA( IS, IL )
                  END IF
               END IF

               IF ( QBITS( 8 ) .EQ. 0 ) THEN
                  IF ( NPIX .LT. 1200 ) THEN
                     NPIX = NPIX + 1
                     SPIX( NPIX ) = IS
                     LPIX( NPIX ) = IL
                     TNPIX( NPIX ) = DATA( IS, IL )
                  END IF

                  IF ( QBITS( 7 ) .NE. 0 ) THEN
                     NSAT = NSAT + 1
                     IF ( NPIX .LE. 1200 ) THEN
                        TYPIX( NPIX ) = 'S'
                     END IF

                  ELSE IF ( QBITS( 6 ) .NE. 0 ) THEN
                     NTRN = NTRN + 1
                     IF ( NPIX .LE. 1200 ) THEN
                        TYPIX(NPIX) = 'T'
                     END IF

                  ELSE IF ( QBITS( 2 ) .NE. 0 ) THEN
                     NUSR = NUSR + 1
                     IF ( NPIX .LE. 1200 ) THEN
                        TYPIX(NPIX) = 'U'
                     END IF

                  ELSE IF ( QBITS( 5 ) .NE. 0 ) THEN
                     NXTR = NXTR + 1
                     IF ( NPIX .LE. 1200 ) THEN
                        TYPIX( NPIX ) = 'E'
                     END IF
                  END IF

               ELSE
                  NRES = NRES + 1
               END IF
            END IF
         END DO
      END DO

*   Print result.
      IF ( NPIX .LE. 0 ) THEN
         CALL LINE_WCONT( '%p%4w No pixels with ITF defects.\\' )
         CALL PRTBUF( STATUS )
         IF ( NRES .LE. 0 ) THEN
            CALL LINE_WCONT(
     :      '%p%4w No pixels affected by Fiducials.\\' )
            CALL PRTBUF( STATUS )

         ELSE
            CALL LINE_WRITI( '%p%4w %i pixels affected by Fiducials.\\',
     :                       NRES )
            CALL PRTBUF( STATUS )
         END IF

      ELSE
         IF ( NPRINT .GE. 2 ) THEN
            N = MIN( NPIX, 1200 )
            CALL LINE_WRITI(
     :           '%p%4w Values of (S,L,FN,Q) for %i pixels:\\', NPIX )
            CALL PRTBUF( STATUS )

            DO IF = 1, N, 3
               IL = MIN( N, IF + 2 )
               CALL LINE_WCONT( '%p%2w \\' )
               DO I = IF, IL
                  CALL LINE_WRITI( '%4w(%3i,\\', SPIX( I ) )
                  CALL LINE_WRITI( '%3i,\\', LPIX( I ) )
                  FN = NINT( REAL( TNPIX( I ) ) * DSCALE + DZERO )
                  CALL LINE_WRITI( '%6i,\\', FN )
                  CALL GEN_CTOS( TYPIX( I ), 2, STR, NCHAR )
                  CALL LINE_WRITS( '%1s)\\', STR )
               END DO

               CALL PRTBUF( STATUS )
            END DO

            CALL LINE_WCONT( '%p%4w end.\\' )
            CALL PRTBUF( STATUS )

            IF ( N .LT. NPIX ) THEN
               CALL LINE_WRITI(
     :                  '%p%4w Only the first %i pixels listed.\\', N )
               CALL PRTBUF( STATUS )
            END IF
         END IF

         CALL LINE_WCONT( '%p%4w Pixel Statistics:\\' )
         CALL PRTBUF( STATUS )
         CALL LINE_WRITI( '%p%6w %6i (S) DN saturated\\', NSAT )
         CALL PRTBUF( STATUS )
         CALL LINE_WRITI( '%p%6w %6i (T) ITF truncated\\', NTRN )
         CALL PRTBUF( STATUS )
         CALL LINE_WRITI( '%p%6w %6i (E) ITF extrapolated\\', NXTR )
         CALL PRTBUF( STATUS )
         CALL LINE_WRITI( '%p%6w %6i (U) marked bad by User\\', NUSR )
         CALL PRTBUF( STATUS )
         CALL LINE_WRITI( '%p%6w %6i (F) Fiducial affected\\', NRES )
         CALL PRTBUF( STATUS )
         CALL LINE_WCONT( '%p%4w end.\\' )
         CALL PRTBUF( STATUS )
      END IF

      IF ( NXTR .GT. 0 ) THEN
         CALL LINE_WCONT(
     :      '%p%4w Since some pixels are in the extrapolated ITF\\' )
         CALL PRTBUF( STATUS )
         CALL LINE_WCONT(
     :         '%p%4w region, this may mean that ITFMAX has been\\' )
         CALL PRTBUF( STATUS )
         CALL LINE_WCONT( '%p%4w given a wrong value.\\' )
         CALL PRTBUF( STATUS )
      END IF

      IFNMAX = NINT( REAL( DSCALE ) * REAL( MAXFN ) + REAL( DZERO ) )
      CALL LINE_WRITI( '%p%4w Maximum FN for unsaturated pixel is %i\\',
     :                 IFNMAX )
      CALL PRTBUF( STATUS )
      CALL LINE_WRITI(
     :        '%p%4w Corresponding to a maximum data number of %i\\',
     :        MAXFN )
      CALL PRTBUF( STATUS )

*   End.
      CALL LINE_WCONT( '%p%2w end.\\' )
      CALL PRTBUF( STATUS )
      CALL PRTEOL( STATUS )

      END
