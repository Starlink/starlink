      SUBROUTINE FV4_SPECIX( XLOC, ISEQ, RPOS, LENGTH, ILOUT, IFAIL,
     :                       STATUS )
*+
*  Name:
*     FV4_SPECIX

*  Purpose:
*     List header information for one spectrum in a file for Specx.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_SPECIX( XLOC, ISEQ, RPOS, LENGTH, ILOUT, IFAIL, STATUS )

*  Description:
*     This routine extracts header information from the SPECX extension
*     located and prints one line to the given Fortran unit. The output
*     can be short or long format. If an error occurs while reading the
*     header information, then this will be reported to the Fortran unit
*     in question, but no status will be set.
*
*     If the scan number turns out to be negative (i.e. the spectrum is
*     deleted), then the scan is simply ignored by this routine.

*  Arguments:
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator to the SPECX extension. In file format version
*        4, files are Starlink Data Files, spectra are cells in an array
*        of NDFs, and headers are SPECX extensions to NDFs.
*     ISEQ = INTEGER (Given)
*        Integer sequence number of spectrum in file. If 0 the original
*        scan number will be substituted.
*     RPOS = INTEGER (Given)
*        XLO, XHI, YLO, YHI: offset boundaries. Only print if DPOS
*        within given boundaries.
*     LENGTH = INTEGER (Given)
*        If this is 2, long format is used for output, else short format
*        is used.
*     ILOUT = INTEGER (Given)
*        The Fortran unit number on which the list file is opened.
*     IFAIL = INTEGER (Given and Returned)
*        The global status.
*     STATUS = INTEGER (Given and Returned)
*        The global Starlink status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     rpt: Remo Tilanus (JAC, Hilo)
*     {enter_new_authors_here}

*  History:
*     07 Dec 1993 (hme):
*        Original version.
*     10 May 1995 (rpt):
*        Added support for ISEQ
*     21 Sep 2000 (ajc):
*        Unused IMODE
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'SPECX_PARS'       ! NQMAX

*  Arguments Given:
      CHARACTER * ( * ) XLOC
      REAL    RPOS ( 4 )
      INTEGER LENGTH
      INTEGER ISEQ
      INTEGER ILOUT

*  Status:
      INTEGER IFAIL              ! Global status
      INTEGER STATUS             ! Starlink status

*  Local Variables:
      INTEGER IOSTAT             ! WRITE error status
      INTEGER I                  ! Loop index
      INTEGER NDIM, NELM         ! Temporary integers
      INTEGER LSCAN, LSEQ
      INTEGER NQUAD
      INTEGER NPTS(  NQMAX )
      INTEGER JFCEN( NQMAX )
      INTEGER JFINC( NQMAX )
      INTEGER INTT
      REAL AZEL( 2 )
      REAL DPOS( 2 )
      DOUBLE PRECISION RADEC( 2 )
      CHARACTER * ( 12 ) RASTR, DECSTR
      CHARACTER * ( 26 ) ITITLE
      CHARACTER * ( 9 ) IDATE
      CHARACTER * ( 8 ) ITIME
      CHARACTER * ( DAT__SZLOC ) TLOC ! An HDS locator

*.

*  Check inherited global status.
      IF ( IFAIL .NE. 0 .OR. STATUS .NE. SAI__OK ) RETURN

*  Get scan number.
      CALL CMP_GET0I( XLOC, 'LSCAN', LSCAN, STATUS )

*  If scan number is positive.
      IF ( LSCAN .GT. 0 ) THEN

*     Print sequence number or scan number.

         LSEQ = ISEQ
         IF ( ISEQ .EQ. 0 ) LSEQ = LSCAN

*     Get the header information.
         CALL DAT_FIND(  XLOC, 'NPTS', TLOC, STATUS )
         CALL DAT_SHAPE( TLOC, 1, NQUAD, NDIM, STATUS )
         CALL DAT_ANNUL( TLOC, STATUS )
         CALL CMP_GET1I( XLOC, 'NPTS',  NQMAX, NPTS,  NELM, STATUS )
         CALL CMP_GET1R( XLOC, 'AZ_EL',     2, AZEL,  NELM, STATUS )
         CALL CMP_GET1I( XLOC, 'JFCEN', NQMAX, JFCEN, NELM, STATUS )
         CALL CMP_GET1I( XLOC, 'JFINC', NQMAX, JFINC, NELM, STATUS )
         CALL CMP_GET0I( XLOC, 'INTT',   INTT,  STATUS )
         CALL CMP_GET1D( XLOC, 'RA_DEC',    2, RADEC, NELM, STATUS )
         CALL CMP_GET1R( XLOC, 'DPOS',      2, DPOS,  NELM, STATUS )
         CALL CMP_GET0C( XLOC, 'ITITLE', ITITLE, STATUS )
         CALL CMP_GET0C( XLOC, 'IDATE',  IDATE,  STATUS )
         CALL CMP_GET0C( XLOC, 'ITIME',  ITIME,  STATUS )

*     Convert RA and Dec to string.
         CALL DEG_TO_STRING( RADEC(1)/15D0, RASTR )
         CALL DEG_TO_STRING( RADEC(2), DECSTR )

*     If header read OK.
         IF ( STATUS .EQ. SAI__OK ) THEN

            IF ( DPOS(1) .GE. RPOS(1)-0.01 .AND.
     :           DPOS(1) .LE. RPOS(2)+0.01 .AND.
     :           DPOS(2) .GE. RPOS(3)-0.01 .AND.
     :           DPOS(2) .LE. RPOS(4)+0.01 ) THEN

*           If long format.
               IF ( LENGTH .EQ. 2 ) THEN

*           Write for first quadrant.
                  WRITE( ILOUT, 101, IOSTAT=IOSTAT )
     :               LSEQ, ITITLE(:26), RASTR, DECSTR(:9), DPOS,
     :               IDATE(:9), ITIME, 1, NPTS(1),
     :               FLOAT(JFCEN(1))/1E6, FLOAT(JFINC(1))/1E6,
     :               FLOAT(INTT)/1E3, NINT(AZEL(2))

*           Write for remaining quadrants.
                  DO 1001 I = 2, MIN( NQUAD, NQMAX )
                     WRITE( ILOUT, 102, IOSTAT=IOSTAT ) I, NPTS(I),
     :                  FLOAT(JFCEN(I))/1E6, FLOAT(JFINC(I))/1E6
 1001             CONTINUE
                  IF ( NQUAD .GT. NQMAX ) WRITE( ILOUT, 103 )

*           Else (short format).
               ELSE


*           Write for first quadrant.
                  WRITE( ILOUT, 105, IOSTAT=IOSTAT )
     :               LSEQ, ITITLE(:18), NPTS(1), 1,
     :               FLOAT(JFCEN(1))/1E6, FLOAT(JFINC(1))/1E6,
     :               DPOS, FLOAT(INTT)/1E3, NINT(AZEL(2))

*           Write for remaining quadrants.
                  DO 1002 I = 2, MIN( NQUAD, NQMAX )
                     WRITE( ILOUT, 106, IOSTAT=IOSTAT ) NPTS(I), I,
     :                  FLOAT(JFCEN(I))/1E6, FLOAT(JFINC(I))/1E6, DPOS
 1002             CONTINUE
                  IF ( NQUAD .GT. NQMAX ) WRITE( ILOUT, 107 )
               END IF

*        Spectrum not within bounds
            ENDIF
*     Else (error while reading header).
         ELSE
            CALL ERR_ANNUL( STATUS )
            WRITE ( ILOUT, 104 )
         END IF
      END IF

*  Formats.
 101  FORMAT( 1X, I4, 1X, A26, 2X, A11, 1X, A9, 1X,
     :   F6.1, 1X, F6.1, 2X, A9, 7X, A8, 1X, I2, 1X,
     :   I5, 2X, F10.6, 2X, F7.4, 2X, F8.1, 2X, I2 )
 102  FORMAT( 1X, 95X, I2, 1X, I5, 2X, F10.6, 2X, F7.4 )
 103  FORMAT( 1X, 95X, '-- FV4_SPECIX -- More quadrants.' )
 104  FORMAT( 1X, '-- FV4_SPECIX -- Error reading header.' )
 105  FORMAT( 1X, I4, 1X, A18, 1X, I5, 1X, I2, 2X, F10.6,
     :   1X, F7.4, 1X, F6.1, 1X, F6.1, 1X, F6.1, 2X, I2 )
 106  FORMAT( 1X, 24X, I5, 1X, I2, 2X, F10.6, 1X, F7.4, 1X, F6.1,
     :   1X, F6.1 )
 107  FORMAT( 1X, 24X, '-- FV4_SPECIX -- More quadrants.' )

*  Return.
      END
