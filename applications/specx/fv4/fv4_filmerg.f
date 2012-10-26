      SUBROUTINE FV4_FILMERG( IFAIL )
*+
*  Name:
*     FV4_FILMERG

*  Purpose:
*     Merges two specx data files - averaging  spectra if necessary

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_FILMERG( IFAIL )

*  Description:
*     This routine is the base routine for the merge
*     Assumes all necessary input/output files are opened
*     (File format version 4).

*  Arguments:
*     IFAIL = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     timj: Tim Jenness (JAC, Hilo)
*     {enter_new_authors_here}

*  History:
*     18 Dec 1995 (timj):
*        Original version.
*     21 Sep 2000 (ajc):
*        Unused NFIL
*      5-NOV-2002 (timj):
*        Correctly call MDATA_READ with STATUS
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Global Variables:
      INCLUDE 'FLAGCOMM'         ! List file unit ILOUT
      INCLUDE 'FILES'            ! Open files information
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:


*  Status:
      INTEGER IFAIL              ! Global status
      INTEGER STATUS             ! Starlink status

*  Local Variables:
      INTEGER MFILE( 3 )         ! 2 inputs plus one output file id
      INTEGER NUMSCAN( 2 )       ! Number of scans in files A & B
      INTEGER NDIM               ! Returned by HDS - not used
      INTEGER ALLSCANS           ! Total number of scans


      INTEGER TJVMSTAT           ! Status from VMGET
      INTEGER*4 IGETVM           ! VM function
      INTEGER*4 IFREEVM          ! VM function
      INTEGER TJIDXPTR           ! Pointer to IDX array
      INTEGER TJOFFPTR           ! Pointer to Offset array
      INTEGER TJCHANNPTR         ! Pointer to channel nums array
      INTEGER TJVLSRPTR          ! Pointer to velocity information
      INTEGER TJNQUADPTR         ! Pointer to NQUAD
      INTEGER TJFREQPTR          ! Pointer to FREQUENCY
      INTEGER TJRADECPTR         ! Pointer to RA,Dec
      INTEGER TJFINCPTR          ! Pointer to FINC

*.

*  Check inherited global status.
      IF ( IFAIL .NE. 0 ) RETURN

*  Begin a new Starlink error context.
      STATUS = SAI__OK
      CALL ERR_MARK

*  Need to find out which files to read from and which to write to

      PRINT *,'Please make sure all necessary files are open'
      WRITE(*,*)

      PRINT *,'First file to read ?'
      CALL GETFIL('R',MFILE(1), IFAIL)
      IF (IFAIL .NE. 0) THEN
         PRINT *, 'Failed to find file. Please check file access'
         IFAIL = 0
         RETURN
      END IF

      PRINT *,'Second file to read ?'
      CALL GETFIL('R',MFILE(2), IFAIL)
      IF (IFAIL .NE. 0) THEN
         PRINT *, 'Failed to find file. Please check file access'
         IFAIL = 0
         RETURN
      END IF

      PRINT *,'Output file ?'
      CALL GETFIL('W',MFILE(3), IFAIL)
      IF (IFAIL .NE. 0) THEN
         PRINT *, 'Failed to find file. Please check file access'
         IFAIL = 0
         RETURN
      END IF


*  Only need to know the shape of the first two arrays
      CALL DAT_SHAPE( SPXLOC(MFILE(1)), 1, NUMSCAN(1), NDIM, STATUS )
      CALL DAT_SHAPE( SPXLOC(MFILE(2)), 1, NUMSCAN(2), NDIM, STATUS )

* Take one off scan numbers (why?)
      NUMSCAN(1) = NUMSCAN(1) - 1
      NUMSCAN(2) = NUMSCAN(2) - 1
      IF ( STATUS .NE. SAI__OK ) THEN
         IFAIL = 38
         GO TO 500
      END IF

* See if the info is correct
CD     PRINT *, 'File A has : ',NUMSCAN(1)
CD     PRINT *, 'File B has : ',NUMSCAN(2)

* Now need to read all the relevant information into an array

* First allocate some memory...
      ALLSCANS = NUMSCAN(1) + NUMSCAN(2)

* Integers are I*2
      TJVMSTAT = IGETVM(16*ALLSCANS, .TRUE., 'MERGE-FILE', TJIDXPTR)
      IF (TJVMSTAT.NE.0) GO TO 500
      TJVMSTAT = IGETVM(4*ALLSCANS, .TRUE., 'MERGE-FILE', TJCHANNPTR)
      IF (TJVMSTAT.NE.0) GO TO 500
      TJVMSTAT = IGETVM(4*ALLSCANS, .TRUE., 'MERGE-FILE', TJNQUADPTR)
      IF (TJVMSTAT.NE.0) GO TO 500
      TJVMSTAT = IGETVM(4*ALLSCANS, .TRUE., 'MERGE-FILE', TJFREQPTR)
      IF (TJVMSTAT.NE.0) GO TO 500
      TJVMSTAT = IGETVM(4*ALLSCANS, .TRUE., 'MERGE-FILE', TJFINCPTR)
      IF (TJVMSTAT.NE.0) GO TO 500

* Reals are R*4
      TJVMSTAT = IGETVM(16*ALLSCANS, .TRUE., 'MERGE-FILE', TJOFFPTR)
      IF (TJVMSTAT.NE.0) GO TO 500
      TJVMSTAT = IGETVM(8*ALLSCANS, .TRUE., 'MERGE-FILE', TJVLSRPTR)
      IF (TJVMSTAT.NE.0) GO TO 500
      TJVMSTAT = IGETVM(64*ALLSCANS, .TRUE., 'MERGE-FILE', TJRADECPTR)
      IF (TJVMSTAT.NE.0) GO TO 500





* Now pass the pointers to subroutine
      CALL MDATA_READ( MFILE, NUMSCAN, ALLSCANS, 
     :                 %VAL(CNF_PVAL(TJOFFPTR)),
     +     %VAL(CNF_PVAL(TJIDXPTR)), %VAL(CNF_PVAL(TJNQUADPTR)), 
     :     %VAL(CNF_PVAL(TJCHANNPTR)),
     +     %VAL(CNF_PVAL(TJVLSRPTR)), %VAL(CNF_PVAL(TJFREQPTR)), 
     :     %VAL(CNF_PVAL(TJFINCPTR)),
     +     %VAL(CNF_PVAL(TJRADECPTR)), IFAIL, STATUS )


* Free up memory
      TJVMSTAT = IFREEVM(TJIDXPTR)
      IF (TJVMSTAT.NE.0) GO TO 500
      TJVMSTAT = IFREEVM(TJOFFPTR)
      IF (TJVMSTAT.NE.0) GO TO 500
      TJVMSTAT = IFREEVM(TJCHANNPTR)
      IF (TJVMSTAT.NE.0) GO TO 500
      TJVMSTAT = IFREEVM(TJNQUADPTR)
      IF (TJVMSTAT.NE.0) GO TO 500
      TJVMSTAT = IFREEVM(TJVLSRPTR)
      IF (TJVMSTAT.NE.0) GO TO 500
      TJVMSTAT = IFREEVM(TJFREQPTR)
      IF (TJVMSTAT.NE.0) GO TO 500
      TJVMSTAT = IFREEVM(TJFINCPTR)
      IF (TJVMSTAT.NE.0) GO TO 500
      TJVMSTAT = IFREEVM(TJRADECPTR)
      IF (TJVMSTAT.NE.0) GO TO 500


*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
      CALL ERR_RLSE
      IFAIL = 0

*  Return.
      END


      SUBROUTINE MDATA_READ( MFILE, NUMSCAN, N, TJOFFS, TJIDX, TJNQUAD,
     +     TJCHANN, TJVLSR, TJFREQ, TJFINC, TJRADEC, IFAIL, STATUS )
*+
*  Name:
*     MDATA_READ

*  Purpose:
*     Reads in header information for comparison

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MDATA_READ( MFILE, NUMSCAN, N, OFFS, IDX, NQUAD, CHANN, LSR,
*                       FREQ, FINC, RADEC, IFAIL, STATUS )

*  Description:
*     Reads and sorts header information from two files
*     Assumes all necessary input/output files are opened
*     (File format version 4).
*     Uses PDA_DSORT, Sorts on offsets of format DX.DY
*     and carries index information with sort so that header info
*     is not lost.
*-
      IMPLICIT NONE

*  Parameters: these parameters determine whether scans are deemed
*              coincident (pos, vel) or not. There use is restricted
*              mostly to the 'REAL' header parameters, INT's are
*              compared directly.
      REAL POSBOX                ! Positional inaccuracy (arcsec) acceptable
      REAL VELBOX                ! Velocity inaccuracy (km/s) acceptable

      PARAMETER (POSBOX=0.05, VELBOX=0.01)

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'SPECX_PARS'       ! NQMAX

*  Global Variables:
      INCLUDE 'FLAGCOMM'         ! List file unit ILOUT
      INCLUDE 'FILES'            ! Open files information

* Arguments
      INTEGER N                  ! Total number of scan
      INTEGER MFILE( 3 )         ! Files ids (#3 is output id)
      INTEGER NUMSCAN( 2 )       ! Number of scans in input files
      INTEGER IFAIL              ! Specx error status
      INTEGER STATUS             ! Starlink status

* Adjustable arrays
      REAL*8  TJIDX(N)           ! Index array
      INTEGER TJNQUAD(N)         ! Number of quadrants
      INTEGER TJCHANN(N)         ! Number of channels
      INTEGER TJFREQ(N)          ! Centre frequency
      INTEGER TJFINC(N)          ! Frequency increment
      REAL    TJOFFS(2,N)        ! Offset array
      REAL    TJVLSR(N)          ! VLSRs
      DOUBLE PRECISION TJRADEC(2,N) ! RA DECS


* Local variables
      INTEGER I, J
      INTEGER FFINC( NQMAX )     ! Frequency increment
      INTEGER FOUND              ! Number of spectra to merge (not inc deleted spectra)
      INTEGER LSCAN
      INTEGER NELM, NDIM, NQUAD
      INTEGER NPTS( NQMAX )
      INTEGER FFREST( NQMAX )
      INTEGER FINISH( 2 ), START( 2 )
      INTEGER CNT, IMINONE
      INTEGER FILEA
      REAL    LLSSRR( 4 )
      REAL    DPOS ( 2 )
      DOUBLE PRECISION RADEC( 2 )
      CHARACTER * ( DAT__SZLOC ) TLOC    ! An HDS locator
      CHARACTER * ( DAT__SZLOC ) XLOC(3) ! An HDS locator

* Some power of 10 larger than largest offset
      REAL MAXOFFSET
      PARAMETER ( MAXOFFSET = 1.0E-4 )

* Now need to read in the header info
*  Check inherited global status.
      IF ( IFAIL .NE. 0 .OR. STATUS .NE. SAI__OK ) RETURN

*     Get the header information.

      START(1) = 1
      FINISH(1) = NUMSCAN(1)
      START(2) = NUMSCAN(1) + 1
      FINISH(2) = N

      PRINT *, 'Reading in header information...'
      FOUND = 0
      DO J = 1, 2
         CNT = 1
         DO I = START(J), FINISH(J)
*     Locate the cell's SPECX extension.
            CALL DAT_CELL( SPXLOC(MFILE(J)), 1, CNT, XLOC(1), STATUS )
            CALL DAT_FIND(  XLOC(1), 'MORE',  XLOC(2), STATUS )
            CALL DAT_FIND(  XLOC(2), 'SPECX', XLOC(3), STATUS )
            CALL DAT_ANNUL( XLOC(1), STATUS )
            CALL DAT_ANNUL( XLOC(2), STATUS )
            CNT = CNT + 1

*     Check if scan has been deleted
            CALL CMP_GET0I( XLOC(3), 'LSCAN', LSCAN, STATUS )
            IF ( LSCAN .GT. 0 ) THEN
               FOUND = FOUND + 1

*     Read in the required info from header
               CALL DAT_FIND( XLOC(3), 'NPTS', TLOC, STATUS )
               CALL DAT_SHAPE( TLOC, 1, NQUAD, NDIM, STATUS )
               CALL DAT_ANNUL( TLOC, STATUS )
               CALL CMP_GET1I( XLOC(3),'NPTS',NQMAX,NPTS,NELM,STATUS)
               CALL CMP_GET1I( XLOC(3),'JFREST',NQMAX,FFREST,NELM,
     :                         STATUS)
               CALL CMP_GET1R( XLOC(3),'DPOS',2,DPOS,NELM,STATUS)
               CALL CMP_GET1R( XLOC(3),'V_SETL',4,LLSSRR,NELM,STATUS)
               CALL CMP_GET1D( XLOC(3), 'RA_DEC',2, RADEC,NELM,STATUS)
               CALL CMP_GET1I( XLOC(3),'JFINC',NQUAD,FFINC,NELM,STATUS)
               CALL DAT_ANNUL(XLOC(3), STATUS)

               IF (STATUS .NE. 0) THEN
                  PRINT *, 'Problem occurred in read, I = ',I, ' J = ',J
                  RETURN
               END IF

C     Transfer all header information to indexed arrays
               TJOFFS(1,FOUND) = DPOS(1)
               TJOFFS(2,FOUND) = DPOS(2)
               TJIDX(FOUND) = REAL(I)
               TJNQUAD(I) = NQUAD
               TJVLSR(I)  = LLSSRR(4)
               TJCHANN(I) = NPTS( 1 )
               TJFREQ(I)  = FFREST( 1 )
               TJFINC(I)  = FFINC( 1 )
               TJRADEC(1,I) = RADEC(1)
               TJRADEC(2,I) = RADEC(2)
            END IF
         END DO

      END DO

C Now need to sort the offsets.... (use Starlink PDA)
C Could it be given a 2-D array of (x,y) to fool it (it uses DBL)? YES!!
      PRINT *, 'Sorting...'
      CALL PDA_DSORT(TJOFFS, TJIDX, FOUND, -2, STATUS)
      PRINT *, ' Sort completed.'

C Uncomment this to test sort output
C      DO I = 1, FOUND
C         PRINT *, INT(TJIDX(I)),TJOFFS(1,I) , TJOFFS(2,I),
C     +        TJRADEC(1,INT(TJIDX(I))), TJRADEC(2,INT(TJIDX(I))),
C     +        TJCHANN(INT(TJIDX(I)))
C      END DO


C Now loop through and compare to make sure data is consistent before merging

      PRINT *, 'Checking data consistency...'
      DO I = 2, FOUND
         CNT = INT(TJIDX(I))
         IMINONE = INT(TJIDX(I-1))
         IF (TJCHANN(CNT) .NE. TJCHANN(IMINONE)) THEN
            PRINT *, 'Number of channels do not agree, ',
     +           'it is unwise to proceed'
            CALL FV4_FILMRGQ(CNT, IMINONE, FINISH(1))

            IFAIL = 1
            RETURN
         ELSE IF (TJFREQ(CNT) .NE. TJFREQ(IMINONE)) THEN
            PRINT *, 'Frequency is different, ',
     +           'it is unwise to proceed'
            CALL FV4_FILMRGQ(CNT, IMINONE, FINISH(1))


            IFAIL = 1
            RETURN
         ELSE IF (TJFINC(CNT) .NE. TJFINC(IMINONE)) THEN
            PRINT *, 'Frequency increment is different, ',
     +           'it is unwise to proceed'
            CALL FV4_FILMRGQ(CNT, IMINONE, FINISH(1))


            IFAIL = 1
            RETURN
         ELSE IF (TJNQUAD(CNT) .NE. TJNQUAD(IMINONE)) THEN
            PRINT *, 'Number of quadrants are different, ',
     +           'it is unwise to proceed'
            CALL FV4_FILMRGQ(CNT, IMINONE, FINISH(1))

            IFAIL = 1
            RETURN
         ELSE IF (ABS(TJVLSR(CNT)-TJVLSR(IMINONE)) .GT. VELBOX) THEN
            PRINT *, 'Velocities are  different, ',
     +           'it is unwise to proceed'
            CALL FV4_FILMRGQ(CNT, IMINONE, FINISH(1))

            IFAIL = 1
            RETURN
         ELSE IF (ABS(TJRADEC(1,CNT)-TJRADEC(1,IMINONE))
     +                .GT. (POSBOX*15.0/3600.0) .OR.
     +            ABS(TJRADEC(2,CNT)-TJRADEC(2,IMINONE))
     +                .GT. (POSBOX/3600.0)) THEN
            PRINT *, 'Map centre is different, ',
     +           'it is unwise to proceed'
            CALL FV4_FILMRGQ(CNT, IMINONE, FINISH(1))


            IFAIL = 1
            RETURN

         END IF
      END DO
      PRINT *, 'Data can be merged!'


C Now loop through and do the merge
C I really want to pull each one on to the stack

C Read first one onto stack....
      IF (INT(TJIDX(1)) .LE. FINISH(1) ) THEN
         FILEA = MFILE(1)
         CNT = TJIDX(1)
      ELSE
         FILEA = MFILE(2)
         CNT = TJIDX(1) - FINISH(1)
      END IF
      CALL FV4_SPECRDTJ( FILEA, CNT, IFAIL )
      IF (IFAIL .NE. 0 ) THEN
         IFAIL = 0
         PRINT *, 'An error occurred during the first read.'
         RETURN
      END IF

C Now add the others
C If there is a match with the previous scan then read it and average
C otherwise write the current spectrum to the merged file

      DO I = 2, FOUND

         CNT = INT(TJIDX(I))
         IMINONE = INT(TJIDX(I-1))

C All of these checks except for offsets are no longer required
C unless the consistency check above is switched off

         IF ( ABS(TJOFFS(1,I)-TJOFFS(1,I-1)) .LE. POSBOX .AND.
     +        ABS(TJOFFS(2,I)-TJOFFS(2,I-1)) .LE. POSBOX .AND.
     +        TJCHANN(CNT) .EQ. TJCHANN(IMINONE) .AND.
     +        ABS(TJRADEC(1,CNT)-TJRADEC(1,IMINONE))
     +                          .LE. (POSBOX*15.0/3600.0) .AND.
     +        ABS(TJRADEC(2,CNT)-TJRADEC(2,IMINONE))
     +                          .LE. (POSBOX/3600.0) .AND.
     +        ABS(TJVLSR(CNT)-TJVLSR(IMINONE)) .LE. VELBOX .AND.
     +        TJFREQ(CNT)  .EQ. TJFREQ(IMINONE)  .AND.
     +        TJFINC(CNT)  .EQ. TJFINC(IMINONE)  .AND.
     +        TJNQUAD(CNT) .EQ. TJNQUAD(IMINONE) ) THEN

C            PRINT *,'Match scans ', CNT,' and ',IMINONE
C What file are they in?
            IF (CNT .LE. FINISH(1) ) THEN
               FILEA = MFILE(1)
               WRITE(ILOUT,*) 'Scan ', CNT, ' in first file'
            ELSE
               FILEA = MFILE(2)
               CNT = CNT - FINISH(1)
               WRITE(ILOUT,*) 'Scan ', CNT, ' in second file'
            END IF
            IF (IMINONE .LE. FINISH(1) ) THEN
               WRITE(ILOUT,*) 'Scan ', IMINONE, ' in first file'
            ELSE
               IMINONE = IMINONE - FINISH(1)
               WRITE(ILOUT,*) 'Scan ', IMINONE, ' in second file'
            END IF
            CALL FV4_SPECRDTJ( FILEA, CNT, IFAIL )
            IF (IFAIL .NE. 0 ) THEN
               IFAIL = 0
               PRINT *, 'An error occurred during read', I
               RETURN
            END IF
            CALL AVGXY(IFAIL)
            IF (IFAIL .NE. 0 ) THEN
               IFAIL = 0
               PRINT *, 'An error occurred in average'
               RETURN
            END IF

         ELSE

C Write current spectrum on stack to file
            CALL WRITESCAN(MFILE(3), IFAIL)
            IF (IFAIL .NE. 0 ) THEN
               IFAIL = 0
               PRINT *, 'An error occurred during write ', I-1
               RETURN
            END IF
C and put next one on
            IF (CNT .LE. FINISH(1) ) THEN
               FILEA = MFILE(1)
            ELSE
               FILEA = MFILE(2)
               CNT = CNT - FINISH(1)
            END IF
            CALL FV4_SPECRDTJ( FILEA, CNT, IFAIL )
            IF (IFAIL .NE. 0 ) THEN
               IFAIL = 0
               PRINT *, 'An error occurred during read ', I
               RETURN
            END IF

         END IF
C Set ifail back to 0
         IF (IFAIL .NE. 0 ) THEN
            IFAIL = 0
            PRINT *, 'An error occurred somewhere. Aborted operation'
            RETURN
         END IF

      END DO

C Write final spectrum to file
      CALL WRITESCAN(MFILE(3), IFAIL)

      END

C Quick sub to work out which file a scan is in

      SUBROUTINE FV4_FILMRGQ( FIRST, SECOND, MIDDLE )

      INTEGER FIRST, SECOND, MIDDLE, DUM

      PRINT *, 'Failed first when comparing :'
      IF (FIRST .LE. MIDDLE ) THEN
         PRINT *, 'Scan', FIRST, ' in first file'
      ELSE
         DUM = FIRST - MIDDLE
         PRINT *, 'Scan ', DUM, ' in second file'
      END IF
      PRINT *,' with...'
      IF (SECOND .LE. MIDDLE ) THEN
         PRINT *, 'Scan', SECOND, ' in first file'
      ELSE
         DUM = SECOND - MIDDLE
         PRINT *, 'Scan ', DUM, ' in second file'
      END IF


      END
