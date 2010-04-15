      SUBROUTINE SPECX_RDFITSSPEC( IFAIL )
*+
*  Name:
*     SPECX_RDFITSSPEC

*  Purpose:
*     Read spectrum from open disk-FITS file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPECX_RDFITSSPEC( IFAIL )

*  Description:
*     This routine serves the READ-FITS-SPECTRUM command of Specx. It
*     reads the spectrum contained in the disk-FITS file that was
*     previously opened with OPEN-FITS-FILE.

*  Arguments:
*     IFAIL = INTEGER (Returned)
*        The global status. The status is reset on entry.

*  Notes:
*     The FITS header must begin with the items SIMPLE, BITPIX, NAXIS,
*     NAXIS1, NAXIS2, ... in that order. SIMPLE must be T, BITPIX must
*     be 8, 16, or 32. Only between 1 and 9 axes may exist. Only one
*     axis is allowed to be significant (longer than 1).

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     timj: Tim Jenness (JAC, Hilo)
*     rpt: Remo Tilanus (JAC, Hilo)
*     ajc: Alan Chipperfield (Starlink,RAL)
*     {enter_new_authors_here}

*  History:
*     27 Jan 1994 (hme):
*        Original version. Synthesis of cody by JF and John Richer on
*        one hand and FIG_FITIN from Portable Figaro.
*     11 Feb 1994 (hme):
*        Must not forget to swap the first data record.
*     11 Mar 1997 (timj):
*        Reset LOFREQ and IFFREQ prior to read.
*     03 May 1999 (rpt):
*        Since Figaro GEN routines for Byte-swap are now tied to
*        host type, byte swap needs to be handled by local subs.
*     06 Jan 2000 (timj):
*        Make compatible with MJD-OBS and Y2K compliant DATE-OBS
*        Initialise ITITLE/IDATE/ITIME so that do not contain leftovers
*        from earlier stack entry.
*      8 May 2000 (ajc):
*        Port to Linux
*        Replace 'TYPE *' with 'PRINT *'
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'STAKPAR'
      INCLUDE 'FLAGCOMM'
      INCLUDE 'STACKCOMM'
      INCLUDE 'SPECX_FITS'

*  Status:
      INTEGER IFAIL              ! Global status

*  Local Variables:
      INTEGER I
      INTEGER ISTAT
      INTEGER HPTR               ! Pointer into buffer
      INTEGER DPTR               ! Pointer into DATA
      INTEGER BITPIX             ! BITPIX value
      INTEGER BLANK              ! BLANK value
      INTEGER NPIX               ! Total number of pixels
      INTEGER NNE1               ! Number of axes longer than 1 pixel
      INTEGER NAXIS              ! NAXIS value
      INTEGER NAXISN( 9 )        ! NAXISn values
      INTEGER RECNO              ! Number of next record in file
      INTEGER STATUS             ! READ status
      REAL RTEMP                 ! Temporary floating point number
      REAL CRPIX1                ! CRPIX1 value
      DOUBLE PRECISION BSCALE    ! BSCALE value
      DOUBLE PRECISION BZERO     ! BZERO value
      DOUBLE PRECISION DTEMP     ! Temporary double precision number
      LOGICAL READUT             ! Read UT from previous keyword
      LOGICAL READDATE           ! Read IDATE from previos keyword
      DOUBLE PRECISION RESTFREQ  ! Local rest frequency value
      DOUBLE PRECISION FRZERO    ! Zeropoint for freq. axis
      DOUBLE PRECISION CRVAL1    ! CRVAL1 value
      DOUBLE PRECISION CDELT1    ! CDELT1 value
      CHARACTER * ( 8 ) KEYWRD   ! A FITS keyword (actual or potential)
      CHARACTER * ( 4 ) VFRAME   ! VFRAME value
      CHARACTER * ( 3 ) VDEF     ! VDEF value

      BYTE        BUFFER( 2880 )
      INTEGER * 2  WBUFF( 1440 )
      INTEGER      IBUFF(  720 )
      CHARACTER * ( 2880 ) HEADER
      CHARACTER * 24 OBS_DATE   ! DATE-OBS string
      EQUIVALENCE ( BUFFER(1), WBUFF(1), IBUFF(1) , HEADER )

*  Internal References:

*  Local Data:

*.

*  Initialise
      READUT = .FALSE.
      READDATE = .FALSE.

*  Reset inherited global status.
      IFAIL = 0

*  Push stack.
      IF ( JTOP .GE. 1 ) THEN
         CALL PUSH()
      ELSE
         JTOP = 1
      END IF

*  Some defaults for item values, in case the items are missing.
      BSCALE = 1.0D0
      BZERO  = 0.0D0
      BLANK  = 2**30 + (2**30 - 1)
      VFRAME = ' '
      VDEF   = ' '
      IFFREQ(1) = 0.0
      LOFREQ(1) = 0.0

*     Initialise title, date and time
      ITITLE = 'FITS'
      IDATE  = '00-JAN-00'
      ITIME  = '00:00:00'

*  Read in the first header record.
*  A record is 2880 bytes, or 36 items with 80 characters each.
      RECNO = 1
      READ( LU, REC=RECNO, IOSTAT=STATUS ) BUFFER
      IF ( STATUS .NE. 0 ) THEN
         WRITE( *, * ) ' Error reading first record.'
         IFAIL = 18
         GO TO 500
      END IF
      RECNO = RECNO + 1
      HPTR = 1

*  Check that the file is 'SIMPLE' FITS.
      IF ( HEADER(1:6)   .NE. 'SIMPLE' .OR.
     :     HEADER(30:30) .NE. 'T'           ) THEN
         WRITE( *, * ) ' Not SIMPLE FITS.'
         IFAIL = 18
         GO TO 500
      END IF
      HPTR = HPTR + 80

*  The second item should be BITPIX. It must be 8, 16, or 32.
      IF ( HEADER(81:86) .NE. 'BITPIX' ) THEN
         WRITE( *, * ) ' Second item should be BITPIX.'
         IFAIL = 18
         GO TO 500
      END IF
      READ( HEADER(91:), *, IOSTAT=STATUS ) BITPIX
      IF (   STATUS .NE. 0          .OR.
     :     ( BITPIX .NE. 8  .AND.
     :       BITPIX .NE. 16 .AND.
     :       BITPIX .NE. 32       )      ) THEN
         WRITE( *, * ) ' Invalid value: BITPIX must be 8, 16, or 32.'
         IFAIL = 18
         GO TO 500
      END IF
      HPTR = HPTR + 80

*  The third item should be NAXIS. It must be less than 10.
      IF ( HEADER(161:165) .NE. 'NAXIS' ) THEN
         WRITE( *, * ) ' Third item should be NAXIS.'
         IFAIL = 18
         GO TO 500
      END IF
      READ( HEADER(171:), *, IOSTAT=STATUS ) NAXIS
      IF ( STATUS .NE. 0 .OR. NAXIS .LT. 1 .OR. NAXIS .GT. 9 ) THEN
         WRITE( *, * ) ' Invalid value: NAXIS must be between 1 and 9.'
         IFAIL = 18
         GO TO 500
      END IF
      HPTR = HPTR + 80

*  Items 4 to 3+NAXIS should be the axis lengths. Only one may be longer
*  than 1.
      NPIX = 1
      NNE1 = 0
      KEYWRD = 'NAXIS1'
      DO 1 I = 1, NAXIS
         WRITE( KEYWRD(6:6), '(I1)' ) I
         IF ( HEADER(161+80*I:166+80*I) .NE. KEYWRD ) THEN
            WRITE( *, * ) ' An NAXISn item is out of place.'
            IFAIL = 18
            GO TO 500
         END IF
         READ( HEADER(171+80*I:), *, IOSTAT=STATUS ) NAXISN(I)
         IF ( STATUS .NE. 0 .OR. NAXISN(I) .LT. 1 ) THEN
            WRITE( *, * ) ' An NAXISn value is invalid.'
            IFAIL = 18
            GO TO 500
         END IF
         IF ( NAXISN(I) .GT. 1 ) NNE1 = NNE1+1
         NPIX = NPIX * NAXISN(I)
         HPTR = HPTR + 80
 1    CONTINUE
      IF ( NPIX .GT. LSTK-128 ) THEN
         PRINT *, ' spectrum longer than stack position -- aborting'
         IFAIL = 18
         GO TO 500
      ELSE IF ( NNE1 .GT. 1 ) THEN
         PRINT *, ' FITS file contains >1 dimensional data -- aborting'
         IFAIL = 18
         GO TO 500
      END IF

*  Read the rest of the header. This includes reading further records if
*  the header exceeds the first record.
*  RECNO is the next record in the file, HPTR is the first character of
*  the next header item.
 2    CONTINUE                 ! Start of endless loop through header

*     Need another record?
         IF ( HPTR .GT. 2880 ) THEN
            READ( LU, REC=RECNO, IOSTAT=STATUS ) BUFFER
            RECNO = RECNO + 1
            HPTR  = 1
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' Error reading a header record.'
               IFAIL = 18
               GO TO 500
            END IF
         END IF

*     Next item the end?
         IF ( HEADER(HPTR:HPTR+2) .EQ. 'END' ) GO TO 3

*     Try all recognisable keywords on the next item.
         KEYWRD = HEADER(HPTR:HPTR+7)
         IF      ( KEYWRD .EQ. 'BLANK'   ) THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) BLANK
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' BLANK item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
         ELSE IF ( KEYWRD .EQ. 'BSCALE'  ) THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) BSCALE
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' BSCALE item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
         ELSE IF ( KEYWRD .EQ. 'BZERO'   ) THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) BZERO
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' BZERO item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
         ELSE IF ( KEYWRD .EQ. 'SCAN-NUM') THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) RTEMP
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' SCAN-NUM item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
            LSCAN = NINT( RTEMP )
            WRITE( ITITLE(1:5), '(I5)', ERR = 901 ) LSCAN
            ITITLE(6:6) = ' '
 901        CONTINUE
         ELSE IF ( KEYWRD .EQ. 'CRVAL2'  ) THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) DTEMP
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' CRVAL2 item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
            RA = DTEMP
         ELSE IF ( KEYWRD .EQ. 'CRVAL3'  ) THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) DTEMP
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' CRVAL3 item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
            DEC = DTEMP
         ELSE IF ( KEYWRD .EQ. 'CDELT2'  ) THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) RTEMP
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' CDELT2 item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
            DRA = RTEMP * 36E2
         ELSE IF ( KEYWRD .EQ. 'CDELT3'  ) THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) RTEMP
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' CDELT3 item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
            DDEC = RTEMP * 36E2
         ELSE IF ( KEYWRD .EQ. 'VLSR'    ) THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) RTEMP
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' VLSR item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
            VLSR = RTEMP / 1E3
         ELSE IF ( KEYWRD .EQ. 'VELO-LSR') THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) RTEMP
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' VELO-LSR item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
            VLSR   = RTEMP / 1E3
            VFRAME = 'LSR'
            VDEF   = 'RAD'
         ELSE IF ( KEYWRD .EQ. 'VELO-HEL') THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) RTEMP
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' VELO-HEL item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
            VLSR   = RTEMP / 1E3
            VFRAME = 'HELI'
            VDEF   = 'OPT'
         ELSE IF ( KEYWRD .EQ. 'VELO-EAR') THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) RTEMP
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' VELO-EAR item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
            VLSR   = RTEMP / 1E3
            VFRAME = 'GEO'
            VDEF   = 'RAD'
         ELSE IF ( KEYWRD .EQ. 'VELO-OBS') THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) RTEMP
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' VELO-OBS item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
            VLSR   = RTEMP / 1E3
            VFRAME = 'TELL'
            VDEF   = 'RAD'
         ELSE IF ( KEYWRD .EQ. 'VEL-FRAM') THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) VFRAME
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' VEL-FRAM item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
         ELSE IF ( KEYWRD .EQ. 'VEL-LAW' ) THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) VDEF
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' VEL-LAW item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
         ELSE IF ( KEYWRD .EQ. 'RESTFREQ') THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) RESTFREQ
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' RESTFREQ item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
         ELSE IF ( KEYWRD .EQ. 'LOFREQ'  ) THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) DTEMP
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' LOFREQ item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
            LOFREQ(1) = DTEMP / 1D9
         ELSE IF ( KEYWRD .EQ. 'IFFREQ'  ) THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) DTEMP
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' IFFREQ item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
            IFFREQ(1) = DTEMP / 1D9
         ELSE IF ( KEYWRD .EQ. 'CRVAL1'  ) THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) CRVAL1
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' CRVAL1 item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
         ELSE IF ( KEYWRD .EQ. 'CDELT1'  ) THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) CDELT1
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' CDELT1 item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
         ELSE IF ( KEYWRD .EQ. 'CRPIX1'  ) THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) CRPIX1
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' CRPIX1 item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
         ELSE IF ( KEYWRD .EQ. 'AZIMUTH' ) THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) RTEMP
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' AZIMUTH item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
            AZ = RTEMP
         ELSE IF ( KEYWRD .EQ. 'ELEVATIO') THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) RTEMP
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' ELEVATIO item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
            EL = RTEMP
         ELSE IF ( KEYWRD .EQ. 'OBSTIME' ) THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) RTEMP
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' OBSTIME item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
            INTT = NINT( RTEMP * 1E3 )
         ELSE IF ( KEYWRD .EQ. 'TSYS'    ) THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) RTEMP
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' TSYS item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
            TSYS(1) = RTEMP
         ELSE IF ( KEYWRD .EQ. 'UT' .AND. .NOT. READUT ) THEN
*     Only read UT keyword if we have not yet read the UT time
*     from either the MJD-OBS keyword or via DATE-OBS
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) ITIME(1:8)
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' UT item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
         ELSE IF (KEYWRD .EQ. 'MJD-OBS') THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) DTEMP
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' MJD-OBS item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
*     Specx stores date and time internally as IDATE and ITIME
*     strings where IDATE is form DD-MON-YY and ITIME is HH:MM:SS.
*     Both these can be obtained via MJD-OBS (and should supercede)
*     the values in UT and DATE-OBS
            CALL DATTIM_FROM_MJD(DTEMP, IDATE, ITIME, STATUS)
            IF (STATUS .NE. 0) THEN
               PRINT *, 'Error calculating date and time from MJD:',
     :              DTEMP, STATUS
               IFAIL = 18
               GO TO 500
            END IF
*     Successfully read date and time so set logicals to prevent
*     UT and DATE-OBS keywords from overriding
            READDATE = .TRUE.
            READUT   = .TRUE.
         ELSE IF ( KEYWRD .EQ. 'DATE-OBS' .AND. .NOT. READDATE) THEN
*     Only read DATE-OBS if we have not previously read the date
*     from an MJD-OBS keyword
            READ ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) OBS_DATE
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ), ' DATE-OBS item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
*     Parse new DATE-OBS -- can obtain ITIME and IDATE from this
*     if we are lucky
            CALL PARSE_DATE_OBS( OBS_DATE, READUT, IDATE, ITIME, STATUS)
            IF ( STATUS .NE. 0 ) THEN
               PRINT *, 'Error parsing DATE-OBS -',OBS_DATE,'-', STATUS
               IFAIL = 18
               GO TO 500
            END IF
            READDATE = .TRUE.
         ELSE IF ( KEYWRD .EQ. 'LINE'    ) THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) ITITLE(7:16)
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' LINE item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
         ELSE IF ( KEYWRD .EQ. 'OBJECT'  ) THEN
            READ( HEADER(HPTR+10:), *, IOSTAT=STATUS ) ITITLE(17:)
            IF ( STATUS .NE. 0 ) THEN
               WRITE( *, * ) ' OBJECT item value is invalid.'
               IFAIL = 18
               GO TO 500
            END IF
         END IF

*     Next item.
         HPTR = HPTR + 80
      GO TO 2                  ! End of endless loop through header
 3    CONTINUE                 ! Escape point from endless loop

*  Header now all read; decode final SPECX parameters
*  First set fixed SPECX parameters not found in IRAM FITS header
      IUTFLG  = 0
      NQUAD   = 1
      NPTS(1) = NPIX

*  Now, patch up the frequency parameters  into specx header:
*
*  The IRAM header seems to provide the parameters:
*  RESTFREQ rest frequency  (Hz)
*  CRVAL1  freq. offset    (Hz)
*  CDELT1  channel spacing (Hz)
*  CRPIX1          a reference channel (number)
*
*
*  Given these, one obtains the freq. of the m-th channel by the
*  linear relation
*
*          freq(m) = RESTFREQ + CRVAL1 + (m-CRPIX1)*CDELT1
*
*  General FITS headers are more likely to provide the absolsute
*  frequency in CRVAL1, hence the above conversion should be done
*  while using 0.0 instead of RESTFREQ.
*
*  Meanwhile, SPECX defines its freq scale by
*     (1)  JFINC(1) is the channel spacing in Hz (INTEGER*4)
*     (2)  JFREST(1) is the rest freq in kHz     (INTEGER*4)
*     (3)  JFCEN(1) is the centre freq in kHz     (INTEGER*4)
*   NOTE: (3) JFCEN means the frequency of the channel #(n+1)/2
*
*  THUS.....
*        the conversion is straightforward, with
*
*         JFCEN = JFREST + CRVAL1 + CDELT1( 0.5 +N/2)-CRPIX1)
*
*  Here we go...
* (The next few lines of code are crucial to getting the velocity scale OK)
      JFINC(1)  = CDELT1
      JFREST(1) = NINT(RESTFREQ/1000.0)
*
* If CRVAL1 is larger than 50 GHz assume that is provides the absolute
* frequency rather than an IRAM offset frequency.
*
      FRZERO = JFREST(1)
      IF (CRVAL1 .GT. 50.0D9) THEN
        FRZERO = 0.0D0
      ENDIF

      RTEMP = 0.5 * FLOAT(NPTS(1)+1)
      RTEMP = RTEMP - CRPIX1
      RTEMP = RTEMP * CDELT1
      JFCEN(1) = FRZERO+NINT(CRVAL1/1000.) + NINT(RTEMP/1000.0)
      IQCEN    = 0

      IF (VFRAME .EQ. ' ') VFRAME = 'LSR'
      IF (VDEF   .EQ. ' ') VDEF   = 'RAD'
      CALL VELENCODE ( VFRAME, VDEF, LSRFLG )

*  Copy data from tape to DATA array. In general we still have part of
*  the last read buffer unused. Can it contain data? No. Data will
*  always at least start at the start of a 2880-byte block. If we had a
*  tape and it used bigger blocks, then we would have to ask ourselves
*  whether the data start in the next 2880-byte logical block or in the
*  next tape block.
*  ====================================================================

*  Read first data record.
      READ( LU, REC=RECNO, IOSTAT=STATUS ) BUFFER
      IF ( STATUS .NE. 0 ) THEN
         WRITE( *, * ) ' Error reading a data record.'
         IFAIL = 18
         GO TO 500
      END IF
      RECNO = RECNO + 1
      HPTR = 1
      DPTR = 1


*  Ask for Byteswap here now instead of in OPEN-FITS.
      BYTESWAP = .FALSE.
       CALL GEN_YESNO( 'Should disk file be byte-reversed?',
     :   BYTESWAP, BYTESWAP, ISTAT )

*  Apply byte swap to first data record: use local versions of
*  the GEN_BSWAP and GEN_WBSWAP routine NOT locked to host type.
      IF ( BYTESWAP .AND. BITPIX .EQ. 16 ) THEN
         CALL BSWAP( WBUFF, 1440 )
      ELSE IF ( BYTESWAP .AND. BITPIX .EQ. 32 ) THEN
         CALL WBSWAP( IBUFF, 720 )
      END IF

*  If 8-bit data.
      IF ( BITPIX .EQ. 8 ) THEN
 4       CONTINUE                 ! Start of endless loop through data

*        Last pixel processed?
            IF ( DPTR .GT. NPIX ) GO TO 5

*        Need another record?
            IF ( HPTR .GT. 2880 ) THEN
               READ( LU, REC=RECNO, IOSTAT=STATUS ) BUFFER
               IF ( STATUS .NE. 0 ) THEN
                  WRITE( *, * ) ' Error reading a data record.'
                  IFAIL = 18
                  GO TO 500
               END IF
               RECNO = RECNO + 1
               HPTR  = 1
            END IF

*        Process pixel. 8-bit data are taken as unsigned.
            DTEMP = BUFFER(HPTR)
            IF ( BUFFER(HPTR) .EQ. BLANK ) THEN
               DATA(DPTR) = BADPIX_VAL
            ELSE IF ( BUFFER(HPTR) .LT. 0 ) THEN
               DATA(DPTR) = BZERO + BSCALE * ( 256D0 + DTEMP )
            ELSE
               DATA(DPTR) = BZERO + BSCALE * DTEMP
            END IF

*        Next pixel.
            HPTR = HPTR + 1
            DPTR = DPTR + 1
         GO TO 4                  ! End of endless loop through data
 5       CONTINUE                 ! Escape point from endless loop

*  Else if 16-bit data.
      ELSE IF ( BITPIX .EQ. 16 ) THEN
 6       CONTINUE                 ! Start of endless loop through data

*        Last pixel processed?
            IF ( DPTR .GT. NPIX ) GO TO 7

*        Need another record?
            IF ( HPTR .GT. 1440 ) THEN
               READ( LU, REC=RECNO, IOSTAT=STATUS ) BUFFER
               IF ( STATUS .NE. 0 ) THEN
                  WRITE( *, * ) ' Error reading a data record.'
                  IFAIL = 18
                  GO TO 500
               END IF
               RECNO = RECNO + 1
               HPTR  = 1
               IF ( BYTESWAP ) CALL BSWAP( WBUFF, 1440 )
            END IF

*        Process pixel. 16-bit data are taken as signed.
            IF ( WBUFF(HPTR) .EQ. BLANK ) THEN
               DATA(DPTR) = BADPIX_VAL
            ELSE
               DATA(DPTR) = BZERO + BSCALE * WBUFF(HPTR)
            END IF

*        Next pixel.
            HPTR = HPTR + 1
            DPTR = DPTR + 1
         GO TO 6                  ! End of endless loop through data
 7       CONTINUE                 ! Escape point from endless loop

*  Else if 32-bit data.
      ELSE IF ( BITPIX .EQ. 32 ) THEN
 8       CONTINUE                 ! Start of endless loop through data

*        Last pixel processed?
            IF ( DPTR .GT. NPIX ) GO TO 9

*        Need another record?
            IF ( HPTR .GT. 720 ) THEN
               READ( LU, REC=RECNO, IOSTAT=STATUS ) BUFFER
               IF ( STATUS .NE. 0 ) THEN
                  WRITE( *, * ) ' Error reading a data record.'
                  IFAIL = 18
                  GO TO 500
               END IF
               RECNO = RECNO + 1
               HPTR  = 1
               IF ( BYTESWAP ) CALL WBSWAP( IBUFF, 720 )
            END IF

*        Process pixel. 32-bit data are taken as signed.
            IF ( IBUFF(HPTR) .EQ. BLANK ) THEN
               DATA(DPTR) = BADPIX_VAL
            ELSE
               DATA(DPTR) = BZERO + BSCALE * IBUFF(HPTR)
            END IF

*        Next pixel.
            HPTR = HPTR + 1
            DPTR = DPTR + 1
         GO TO 8                  ! End of endless loop through data
 9       CONTINUE                 ! Escape point from endless loop
      END IF

*  Tidy up.
 500  CONTINUE
      IF ( IFAIL .NE. 0 ) CALL POP()

      END
