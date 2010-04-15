      SUBROUTINE dasmerge( NDROP, ADJQUAD, WIDEBAND, IFAIL )
*+
* Name:
*     dasmerge.f
*
* Purpose:
*     Implement RPT's das-merge.spx macro in fortran to speed
*     up reading of raster files.
*
* Language:
*     FORTRAN 77 (Sun)
*
* Invocation:
*     CALL DASMERGE( NDROP, ADJQUAD, WIDEBAND, IFAIL )
*
* Description:
*     This is just Remo's dasmerge.spx command implemented in FORTRAN
*
* Arguments:
*
*     NDROP     - INTEGER  (returned)
*         Number of channels to be dropped (only used by READ-GSD-RASTER)
*     ADJQUAD   - LOGICAL  (returned)
*         Adjust qudrant DC offset in merge? (only used by READ-GSD-RASTER)
*     WIDEBAND  - LOGICAL  (returned)
*         Wideband mode? If so, use X and Y  (only used by READ-GSD-RASTER)
*     IFAIL     - INTEGER  (given and returned)
*         Status flag
*
* Prior Requirements:
*     Must be a scan on the stack
*
* Authors:
*     rpt: Remo Tilanus (JACH)
*     timj:Tim Jenness  (JACH)
*     {enter_new_authors_here}

* History:
*     Ages ago (rpt):
*          dasmerge.spx script written
*     14 Dec 1995 (timj):
*          converted to FORTRAN
*      4 Jun 1997 (rpt):
*          added poor-mans support for wideband: uses concatsp
*          and three merges rather than one merge on all quadrants
*     12 April 2004 (timj):
*          Tidy up header
*     14 April 2004 (timj):
*          Make sure that we do not suffer from the problem whereby
*          an identical variable can be supplied as an argument for
*          ADJQUAD and WIDEBAND.
*     {enter_further_changes_here}

*
*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

* Returned variables:
      INTEGER NDROP             ! Number of channels to drop
      LOGICAL ADJQUAD           ! Adjust quadrant offsets?
      LOGICAL WIDEBAND          ! Wideband mode?


* Local variables:
      INTEGER FINC               ! Frequency increment (Hz)
      INTEGER FI                 ! DUMMY
      INTEGER NOVER              ! Size of overlap
      INTEGER NUSE               ! Half number of channels to drop
      INTEGER JDEF               ! Returned by GENLIB
      REAL*8  FEDGE(2)           ! Frequency boundaries of overlap
      LOGICAL LADJQUAD           ! Local version of ADJQUAD
      LOGICAL LWIDEBAND          ! Local version of WIDEBAND

* Given and returned:
      INTEGER IFAIL              ! Global error


* Global constants
      INCLUDE 'STACKCOMM'        ! Spectrum header
      INCLUDE 'FLAGCOMM'         ! Needed for ILOUT


      IF (IFAIL .NE. 0) RETURN

      JDEF = 0
      ADJQUAD = .FALSE.
      WIDEBAND = .FALSE.

      IF (NQUAD .GT. 1) THEN

         FINC = ABS(JFINC(1))

         IF (NQUAD .EQ. 2) THEN
            FI = 2
         ELSE
            FI = NQUAD / 2
         END IF

         FEDGE(1) = ( 1000.0 * REAL( JFCEN(FI-1) ) ) +
     +        ( REAL(FINC) * REAL( NPTS(FI-1) - 1 ) / 2.0 )
         FEDGE(2) = ( 1000.0 * REAL(JFCEN(FI)) ) -
     +        ( REAL(FINC) * REAL( NPTS(FI) - 1 ) / 2.0 )

         NOVER = INT((ABS( FEDGE(2) - FEDGE(1) ) / REAL(FINC)) + 0.5 )

         write(ilout, 10) int(nover)
 10      format('There are ',i4,' overlapping channels')

         NUSE = INT(0.5 * NOVER)
         CALL GEN_GETI4('Number of overlap channels to use? ',
     +        NUSE,'I4',NUSE,JDEF)
         JDEF = 0
         CALL GEN_YESNO ('Adjust any DC offset quadrants? ', .FALSE.,
     +        LADJQUAD, JDEF)
         JDEF = 0
         CALL GEN_YESNO ('Wideband mode (also merge Y spectrum)? ',
     +        .FALSE., LWIDEBAND, JDEF)

C         print *,'NUSE = ',NUSE
C         IF (LADJQUAD) print *,'ADJ quad is true'
C         IF (LWIDEBAND) print *,'Wideband mode is true'

C Call the core dasmerge subroutine
         IF (NUSE .LE. NOVER .AND. NUSE .GT. 0) THEN
            NDROP = (NOVER - NUSE) /2
         ELSE IF (NUSE .GT. NOVER) THEN
            WRITE(ILOUT,*) 'WARNING! You are trying to use more',
     +           ' channels than there are!'
            WRITE(ILOUT,*) 'Using all the channels anyway'
            NDROP = 0
         ELSE IF (NUSE .LT. 0) THEN
            WRITE(ILOUT,*) 'WARNING! Attempting to throw away all',
     +           ' overlap channels!'
            WRITE(ILOUT,*) 'Using 1 overlap channel'
            NDROP = (NOVER - 2 )/ 2
         END IF

C         PRINT *, 'NDROP = ',NDROP
C         IF (.NOT.LADJQUAD) print *,'ADJ quad is false'

         CALL DODASMERGE(NDROP, LADJQUAD, LWIDEBAND, IFAIL)


      ELSE
         WRITE(ILOUT,*) 'Spectrum only has one quadrant'
      END IF

C     Copy logicals to output variable. Do it this way to protect us
C     from being called with two identical dummy variables
      WIDEBAND = LWIDEBAND
      ADJQUAD = LADJQUAD

      END

C---------------------------------------------------------------------
* Purpose:
*     Core fortran routine for DAS-MERGE
*
* Language:
*     FORTRAN 77 (Sun)
*
* Invocation:
*     CALL DODASMERGE( NDROP, ADJQUAD, WIDEBAND, IFAIL )
*
* Description:
*     This is just Remo's dasmerge.spx command implemented in FORTRAN
*
* Arguments:
*     NDROP       -     Number of channels to drop (INTEGER)
*     ADJQUAD     -     Whether to adjust quadrant offsets (LOGICAL)
*     WIDEBAND    -     Whether in wideband mode and to merge Y (LOGICAL)
*     IFAIL       -     Status (INTEGER)
*
* Prior Requirements:
*     Must be a scan on the stack
*
*-

      SUBROUTINE DODASMERGE( NDROP , ADJQUAD, WIDEBAND, IFAIL )

      IMPLICIT NONE

*  Arguments Given
      INTEGER NDROP
      LOGICAL ADJQUAD
      LOGICAL WIDEBAND

*  Arguments Given & Returned
      INTEGER IFAIL

*  Global variables
      INCLUDE 'FLAGCOMM'
      INCLUDE 'SPECX_PARS'
      REAL XSCALE (2*LSPMAX)

*  Local Variables
      INTEGER NQ
      INTEGER ICHECK
      INTEGER ISLCTQ

*  External subroutine
      EXTERNAL MERGE

C First check status
      IF(ICHECK(1,IFAIL).NE.1) THEN
         WRITE(ILOUT, *) 'IFAIL error - DAS-MERGE failed'
         RETURN
      END IF

C Quadrant status
      IF(ISLCTQ(NQ,IFAIL).NE.1)   RETURN

C Set DC adjust mode for merge
      SECTOR_OFFSET = ADJQUAD

C First run the truncate command, then merge
      CALL TRUNC(NQ, NDROP)
      CALL MERGE(XSCALE,IFAIL)

C If WIDEBAND mode repeat for the Y spectrum
      IF(WIDEBAND) THEN
         CALL XY
         CALL TRUNC(NQ, NDROP)
         CALL MERGE(XSCALE,IFAIL)
         CALL XY

C Now finally concatinate X and Y and do a final merge

         CALL CONCATSP(IFAIL)
         CALL MERGE(XSCALE,IFAIL)

      ENDIF

      END
