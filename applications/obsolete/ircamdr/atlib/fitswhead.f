	SUBROUTINE FITSWHEAD( TAPEORDISK, MTCHAN, LUNO, DIMSX, DIMSY,
     :	                      BZERO, BSCALE, ARRAY, INST, PLATESCALE,
     :	                      OBSERVERS, ORIGIN, SOFTWARE, HEIGHT, LAT,
     :                        LONG, TELESCOPE, BIAS, CONF, MAX, MIN,
     :	                      DEC, AIRMASS, EQUINOX, EVENMEAN, EVENSTD,
     :	                      EXPO, FILTER, GAIN, GATE, LOCALTIME, MODE,
     :	                      COADDS, OBJECT, ODDMEAN, ODDSTD,
     :	                      OFFSETDEC, OFFSETRA, RA, READRATE, TEMP,
     :	                      TITLE, IMAGENAME, UT, HA, ST, FPX, FPY,
     :	                      FPZ, XHEAD_ARCSECPMM, XHEAD_DEC_ZERO,
     :	                      XHEAD_RA_ZERO, XHEAD_DEC, XHEAD_RA,
     :                        COMMENTS)

* Subroutine to take the values defining an observation image and write them
* to tape in a FITS format header.

*     HISTORY
*     14-JUL-1994  Changed STR$ to CHR_ (SKL@JACH)
*      9-AUG-2004  Turn on IMPLICIT and fix TEXTD error (TIMJ@JACH)

	IMPLICIT NONE

        INCLUDE 'CHR_ERR'



	INTEGER
     :	  MAXNVALS            ! maximum number of values in NVALS array

	PARAMETER ( MAXNVALS = 2880)

	BYTE
     :	  NVALS( MAXNVALS)    ! array for packed header values

	INTEGER
     :	  DIMSX,              ! X dimensions of current image
     :	  DIMSY,              ! Y dimensions of current image
     :	  ENDCARD,            ! number of last card to contain END
     :	  IJUNK,              ! temporary integer variable
     :	  LUNO,               ! lun for output disk fits file
     :	  LEN,                ! length of character string
     :	  LEN2,               ! length of character string
     :	  MTCHAN,             ! channel number for MT writes
     :	  MAXCARDS,           ! maximum number of card images per record
     :	  MAXCHARS,           ! maximum number characters per card image
     :	  NBITS,              ! number of bits per data pixel value
     :	  NVALSINDEX          ! index for access into array NVALS

	PARAMETER ( NBITS = 32)
	PARAMETER ( MAXCARDS = 72)
	PARAMETER ( MAXCHARS = 80)

	REAL
     :	  AIRMASS,            ! Airmass
     :	  BIAS,               ! Bias voltage on chip (mV)
     :	  BSCALE,             ! scale for conversion back to 32 bit data (DN)
     :	  BZERO,              ! zero for conversion back to 32 bit data (DN)
     :	  COADDS,             ! number of coadds in data (ind)
     :	  DEC( 3),            ! declination of telescope (D,M,S)
     :    RDEC,               ! dec in decimal
     :	  EQUINOX,            ! equinox of telescope coordinates (Y)
     :	  HEIGHT,             ! height above sea level of telescope (FT)
     :	  EVENMEAN,           ! stats mean in even pixels (DN)
     :	  EVENSTD,            ! stats std in even pixels (DN)
     :	  EXPO,               ! exposure time on chip (mS)
     :	  FPX,                ! FP X
     :	  FPY,                ! FP Y
     :	  FPZ                 ! FP Z
      REAL
     :	  GAIN,               ! system gain factor (ind)
     :	  GATE,               ! gate voltage on chip (V)
     :	  HA( 3),             ! Hour Angle
     :	  JUNK,               ! junk variable
     :	  LAT( 3),            ! latitude of telescope (D,M,S)
     :    RLAT, RLONG,        ! Long and lat in decimal
     :	  LONG( 3),           ! longitude of telescope (D,M,S)
     :	  MAX,                ! maximum value in data image (DN)
     :	  MIN,                ! minimum value in data image (DN)
     :	  ODDMEAN,            ! stats mean in odd pixels (DN)
     :	  ODDSTD,             ! stats std in odd pixels (DN)
     :	  OFFSETDEC,          ! offset in dec from DEC
     :	  OFFSETRA,           ! offset in ra from RA
     :	  PLATESCALE          ! plate scale of image ("/pix)
      REAL
     :	  RA( 3),             ! ra of telescope (H,M,S)
     :	  READRATE,           ! readout rate of array (mS)
     :	  RRA,                ! real ra
     :	  RUT,                ! real ut
     :	  RHA,                ! real ha
     :	  RST,                ! real st
     :    ST( 3),             ! sidereal time
     :	  TEMP,               ! temperature of array (K)
     :	  UT( 4),             ! read ut
     :	  XHEAD_ARCSECPMM,    ! xhead arcsec/mm scale
     :	  XHEAD_DEC_ZERO,     ! xhead dec zero point
     :	  XHEAD_RA_ZERO,      ! xhead ra zero point
     :	  XHEAD_DEC,          ! xhead dec position
     :	  XHEAD_RA            ! xhead ra position


	CHARACTER
     :	  ARRAY*( *),         ! array used for observation
     :	  CJUNK*80,           ! temporary character variable
     :	  COMMENTS( 5)*80,    ! comments
     :	  CONF*( *),          ! configuration of observation
     :	  FILTER*( *),        ! filter used in observation
     :	  IMAGENAME*( *),     ! name of image written tape/file
     :	  INST*( *),          ! instrument used in observation
     :	  CARD( MAXCARDS)*120, ! card images
     :	  TEXT( MAXCARDS)*40,  ! card images
     :	  LOCALTIME*( *),     ! local date/time of observation (end)
     :	  MODE*( *),          ! mode of observation
     :	  OBJECT*( *),        ! object name
     :	  OBSERVERS*( *)      ! observers
      CHARACTER
     :	  ORIGIN*( *),        ! origin of observers
     :	  SOFTWARE*( *),      ! software written by
     :	  TAPEORDISK*( *),    ! write to tape or disk
     :	  TELESCOPE*( *),     ! telescope name
     :	  TITLE*( *),         ! title of observations
     :	  DATE1*2,            ! date day
     :	  DATE2*3,            ! date month
     :	  DATE3*2,            ! date year
     :	  RDATE2*2,           ! real date month
     :	  TIME*8              ! time

	INTEGER JCHAR, JTAPE, JCARD
	INTEGER MTWR
	EXTERNAL MTWR

*      Start defining the cards : data format type
	CARD( 1) = 'SIMPLE  ='
	CARD( 1)( 29:30) = ' T'
	TEXT( 1) = ' / FITS Format'

*      Bits per image pixel
	CARD( 2) = 'BITPIX  ='
	WRITE( CJUNK, '(I20)') NBITS
        CALL CHR_CLEAN( CARD( 2) )
        LEN = 0
	CALL CHR_APPND( CARD( 2), CARD( 2), LEN)
	CARD( 2) = CARD( 2)( 1:LEN) // ' ' // CJUNK
	TEXT( 2) = ' / Bits/pixel'

*      Number of dimensions in data image
	IJUNK = 2
	CARD( 3) = 'NAXIS   ='
	WRITE( CJUNK, '(I20)') IJUNK
        CALL CHR_CLEAN( CARD( 3) )
        LEN = 0
	CALL CHR_APPND( CARD( 3), CARD( 3), LEN)
	CARD( 3) = CARD( 3)( 1:LEN) // ' ' // CJUNK
	TEXT( 3) = ' / Number of Axes'

*      Dimensions of first image axis
	CARD( 4) = 'NAXIS1  ='
	WRITE( CJUNK, '(I20)') DIMSX
        CALL CHR_CLEAN( CARD( 4) )
        LEN = 0
	CALL CHR_APPND( CARD( 4), CARD( 4), LEN)
	CARD( 4) = CARD( 4)( 1:LEN) // ' ' // CJUNK
	TEXT( 4) = ' / 1st Dimension'

*      Dimensions of second image axis
	CARD( 5) = 'NAXIS2  ='
	WRITE( CJUNK, '(I20)') DIMSY
        CALL CHR_CLEAN( CARD( 5) )
        LEN = 0
	CALL CHR_APPND( CARD( 5), CARD( 5), LEN)
	CARD( 5) = CARD( 5)( 1:LEN) // ' ' // CJUNK
	TEXT( 5) = ' / 2nd Dimension'

*      Calculated zero point for image rescaling
	CARD( 6) = 'BZERO   ='
	WRITE( CJUNK, '(F20.6)') BZERO
        CALL CHR_CLEAN( CARD( 6) )
        LEN = 0
	CALL CHR_APPND( CARD( 6), CARD( 6), LEN)
	CARD( 6) = CARD( 6)( 1:LEN) // ' ' // CJUNK
	TEXT( 6) = ' / REAL = TAPE*BSCALE + BZERO'

*      Calculated scaler for image rescaling
	CARD( 7) = 'BSCALE  ='
	WRITE( CJUNK, '(F20.6)') BSCALE
        CALL CHR_CLEAN( CARD( 7) )
        LEN = 0
	CALL CHR_APPND( CARD( 7), CARD( 7), LEN)
	CARD( 7) = CARD( 7)( 1:LEN) // ' ' // CJUNK
	TEXT( 7) = ' / REAL = TAPE*BSCALE + BZERO'

*      Telescope name
	CARD( 8) = 'TELESCOP='
        CALL CHR_CLEAN( TELESCOPE )
        LEN = 0
	CALL CHR_APPND( TELESCOPE, TELESCOPE, LEN)
	IF( LEN .LT. 8) THEN
	  TELESCOPE( LEN+1:8) = ' '
	  LEN = 8
	END IF
	CJUNK = CHAR( 39) // TELESCOPE( 1:LEN) // CHAR( 39)
        CALL CHR_CLEAN( CARD( 8) )
        LEN = 0
	CALL CHR_APPND( CARD( 8), CARD( 8), LEN)
	CARD( 8) = CARD( 8)( 1:LEN) // ' ' // CJUNK
	TEXT( 8) = ' / Telescope name'

*      Instrument used
	CARD( 9) = 'INSTRUME='
        CALL CHR_CLEAN( INST )
        LEN = 0
	CALL CHR_APPND( INST, INST, LEN)
	IF( LEN .LT. 8) THEN
	  INST( LEN+1:8) = ' '
	  LEN = 8
	END IF
	CJUNK = CHAR( 39) // INST( 1:LEN) // CHAR( 39)
        CALL CHR_CLEAN( CARD( 9) )
        LEN = 0
	CALL CHR_APPND( CARD( 9), CARD( 9), LEN)
	CARD( 9) = CARD( 9)( 1:LEN) // ' ' // CJUNK
	TEXT( 9) = ' / Instrument name'

*      Plate scale of image
	CARD( 10) = 'PIXSCALE='
	WRITE( CJUNK, '(F20.3)') PLATESCALE
        CALL CHR_CLEAN( CARD( 10) )
        LEN = 0
	CALL CHR_APPND( CARD( 10), CARD( 10), LEN)
	CARD( 10) = CARD( 10)( 1:LEN) // ' ' // CJUNK
	TEXT( 10) = ' / Nominal arcseconds/pixel'

*      Local contacts
	CARD( 11) = 'CONTACTS='
        CALL CHR_CLEAN( CARD( 11) )
        LEN = 0
	CALL CHR_APPND( CARD( 11), CARD( 11), LEN)
	CARD( 11) = CARD( 11)( 1:LEN) // ' ' //
     :	  CHAR( 39) // 'Mark Casali/Colin Aspin' // CHAR( 39)
	TEXT( 11) = ' / Local contacts'

*      Observers names
	CARD( 12) = 'OBSERVER='
        CALL CHR_CLEAN( OBSERVERS )
        LEN = 0
	CALL CHR_APPND( OBSERVERS, OBSERVERS, LEN)
	IF( LEN .LT. 8) THEN
	  OBSERVERS( LEN+1:8) = ' '
	  LEN = 8
	END IF
	CJUNK = CHAR( 39) // OBSERVERS( 1:LEN) // CHAR( 39)
        CALL CHR_CLEAN( CARD( 12) )
        LEN = 0
	CALL CHR_APPND( CARD( 12), CARD( 12), LEN)
	CARD( 12) = CARD( 12)( 1:LEN) // ' ' // CJUNK
	TEXT( 12) = ' / Observers names'

*      Observers origins
	CARD( 13) = 'OBSORIGI='
        CALL CHR_CLEAN( ORIGIN )
        LEN = 0
	CALL CHR_APPND( ORIGIN, ORIGIN, LEN )
	IF( LEN .LT. 8) THEN
	  ORIGIN( LEN+1:8) = ' '
	  LEN = 8
	END IF
	CJUNK = CHAR( 39) // ORIGIN( 1:LEN) // CHAR( 39)
        CALL CHR_CLEAN( CARD( 13) )
        LEN = 0
	CALL CHR_APPND( CARD( 13), CARD( 13), LEN)
	CARD( 13) = CARD( 13)( 1:LEN) // ' ' // CJUNK
	TEXT( 13) = ' / Observers origin'

*      Software written by
	CARD( 14) = 'SOFTWARE='
        CALL CHR_CLEAN( SOFTWARE )
        LEN = 0
	CALL CHR_APPND( SOFTWARE, SOFTWARE, LEN)
	IF( LEN .LT. 8) THEN
	  SOFTWARE( LEN+1:8) = ' '
	  LEN = 8
	END IF
	CJUNK = CHAR( 39) // SOFTWARE( 1:LEN) // CHAR( 39)
        CALL CHR_CLEAN( CARD( 14) )
        LEN = 0
	CALL CHR_APPND( CARD( 14), CARD( 14), LEN)
	CARD( 14) = CARD( 14)( 1:LEN) // ' ' // CJUNK
	TEXT( 14) = ' / Software source'

*      Height of telescope
	CARD( 15) = 'HEIGHT  ='
	WRITE( CJUNK, '(F20.3)') HEIGHT
        CALL CHR_CLEAN( CARD( 15) )
        LEN = 0
	CALL CHR_APPND( CARD( 15), CARD( 15), LEN)
	CARD( 15) = CARD( 15)( 1:LEN) // ' ' // CJUNK
	TEXT( 15) = ' / Height in feet'

*      Latitude of telescope
	RLAT = LAT( 1)+LAT( 2)/60.0+LAT( 3)/3600.0
	CARD( 16) = 'LATITUDE='
	WRITE( CJUNK, '(F20.6)') RLAT
        CALL CHR_CLEAN( CARD( 16) )
        LEN = 0
	CALL CHR_APPND( CARD( 16), CARD( 16), LEN)
	CARD( 16) = CARD( 16)( 1:LEN) // ' ' // CJUNK
	TEXT( 16) = ' / Latitude in degrees'

*      Longitude of telescope
	RLONG = -1.0*( LONG( 1)+LONG( 2)/60.0+LONG( 3)/3600.0)
	CARD( 17) = 'LONGITUD='
	WRITE( CJUNK, '(F20.6)') RLONG
        CALL CHR_CLEAN( CARD( 17) )
        LEN = 0
	CALL CHR_APPND( CARD( 17), CARD( 17), LEN)
	CARD( 17) = CARD( 17)( 1:LEN) // ' ' // CJUNK
	TEXT( 17) = ' / Longitude in degrees'

*      Array type used
	CARD( 18) = 'ARRAYTYP='
        CALL CHR_CLEAN( ARRAY )
        LEN = 0
	CALL CHR_APPND( ARRAY, ARRAY, LEN)
	IF( LEN .LT. 8) THEN
	  ARRAY( LEN+1:8) = ' '
	  LEN = 8
	END IF
	CJUNK = CHAR( 39) // ARRAY( 1:LEN) // CHAR( 39)
        CALL CHR_CLEAN( CARD( 18) )
        LEN = 0
	CALL CHR_APPND( CARD( 18), CARD( 18), LEN)
	CARD( 18) = CARD( 18)( 1:LEN) // ' ' // CJUNK
	TEXT( 18) = ' / Array used'

*      Configuration of observation : Stare or Chop
	CARD( 19) = 'CONFIGUR='
        CALL CHR_CLEAN( CONF )
        LEN = 0
	CALL CHR_APPND( CONF, CONF, LEN)
	IF( LEN .LT. 8) THEN
	  CONF( LEN+1:8) = ' '
	  LEN = 8
	END IF
	CJUNK = CHAR( 39) // CONF( 1:LEN) // CHAR( 39)
        CALL CHR_CLEAN( CARD( 19) )
        LEN = 0
	CALL CHR_APPND( CARD( 19), CARD( 19), LEN)
	CARD( 19) = CARD( 19)( 1:LEN) // ' ' // CJUNK
	TEXT( 19) = ' / Observing configuration'

*      Mode of observation : KTC On or Off
	CARD( 20) = 'MODE    ='
        CALL CHR_CLEAN( MODE )
        LEN = 0
	CALL CHR_APPND( MODE, MODE, LEN)
	IF( LEN .LT. 8) THEN
	  MODE( LEN+1:8) = ' '
	  LEN = 8
	END IF
	CJUNK = CHAR( 39) // MODE( 1:LEN) // CHAR( 39)
        CALL CHR_CLEAN( CARD( 20) )
        LEN = 0
	CALL CHR_APPND( CARD( 20), CARD( 20), LEN)
	CARD( 20) = CARD( 20)( 1:LEN) // ' ' // CJUNK
	TEXT( 20) = ' / Observing mode'

*      System gain
	CARD( 21) = 'SYSTGAIN='
	WRITE( CJUNK, '(F20.3)') GAIN
        CALL CHR_CLEAN( CARD( 21) )
        LEN = 0
	CALL CHR_APPND( CARD( 21), CARD( 21), LEN)
	CARD( 21) = CARD( 21)( 1:LEN) // ' ' // CJUNK
	TEXT( 21) = ' / Electronic gain factor'

*      Total gain
	CARD( 22) = 'ELPERDN ='
	JUNK = 25.0
	WRITE( CJUNK, '(F20.3)') JUNK
        CALL CHR_CLEAN( CARD( 22) )
        LEN = 0
	CALL CHR_APPND( CARD( 22), CARD( 22), LEN)
	CARD( 22) = CARD( 22)( 1:LEN) // ' ' // CJUNK
	TEXT( 22) = ' / Nominal electrons to DN conversion'

*      Bias voltage on array
	CARD( 23) = 'BIASVOLT='
	WRITE( CJUNK, '(F20.3)') BIAS
        CALL CHR_CLEAN( CARD( 23) )
        LEN = 0
	CALL CHR_APPND( CARD( 23), CARD( 23), LEN)
	CARD( 23) = CARD( 23)( 1:LEN) // ' ' // CJUNK
	TEXT( 23) = ' / Bias voltage in mVolts'

*      Gate voltage on array
	CARD( 24) = 'GATEVOLT='
	WRITE( CJUNK, '(F20.3)') GATE
        CALL CHR_CLEAN( CARD( 24) )
        LEN = 0
	CALL CHR_APPND( CARD( 24), CARD( 24), LEN)
	CARD( 24) = CARD( 24)( 1:LEN) // ' ' // CJUNK
	TEXT( 24) = ' / Gate voltage in Volts'

*      Object name
	CARD( 25) = 'OBJECT  ='
        CALL CHR_CLEAN( OBJECT )
        LEN = 0
	CALL CHR_APPND( OBJECT, OBJECT, LEN )
	IF( LEN .LT. 8) THEN
	  OBJECT( LEN+1:8) = ' '
	  LEN = 8
	END IF
	CJUNK = CHAR( 39) // OBJECT( 1:LEN) // CHAR( 39)
        CALL CHR_CLEAN( CARD( 25) )
        LEN = 0
	CALL CHR_APPND( CARD( 25), CARD( 25), LEN)
	CARD( 25) = CARD( 25)( 1:LEN) // ' ' // CJUNK
	TEXT( 25) = ' / Object name'

*      Title of observation
        CALL CHR_CLEAN( TITLE )
        LEN = 0
	CALL CHR_APPND( TITLE, TITLE, LEN)
	IF( LEN .LT. 8) THEN
	  TITLE( LEN+1:8) = ' '
	  LEN = 8
	END IF
	CJUNK = CHAR( 39) // TITLE( 1:LEN) // CHAR( 39)
        CALL CHR_CLEAN( CARD( 26) )
        LEN = 0
	CALL CHR_APPND( CARD( 26), CARD( 26), LEN)
	CARD( 26) = CARD( 26)( 1:LEN) // ' ' // CJUNK
	TEXT( 26) = ' / Observation title'

*      Maximum in data image
	CARD( 27) = 'MAXIMUM ='
	WRITE( CJUNK, '(F20.3)') MAX
        CALL CHR_CLEAN( CARD( 27) )
        LEN = 0
	CALL CHR_APPND( CARD( 27), CARD( 27), LEN)
	CARD( 27) = CARD( 27)( 1:LEN) // ' ' // CJUNK
	TEXT( 27) = ' / Maximum in data (DN)'

*      Minimum in data image
	CARD( 28) = 'MINIMUM ='
	WRITE( CJUNK, '(F20.3)') MIN
        CALL CHR_CLEAN( CARD( 28) )
        LEN = 0
	CALL CHR_APPND( CARD( 28), CARD( 28), LEN)
	CARD( 28) = CARD( 28)( 1:LEN) // ' ' // CJUNK
	TEXT( 28) = ' / Minimum in data (DN)'

*      Odd channel mean in statistics
	CARD( 29) = 'ODDMEAN ='
	WRITE( CJUNK, '(F20.3)') ODDMEAN
        CALL CHR_CLEAN( CARD( 29) )
        LEN = 0
	CALL CHR_APPND( CARD( 29), CARD( 29), LEN)
	CARD( 29) = CARD( 29)( 1:LEN) // ' ' // CJUNK
	TEXT( 29) = ' / Odd channel mean (DN)'

*      Odd channel standard deviation in statistics
	CARD( 30) = 'ODDSTD  ='
	WRITE( CJUNK, '(F20.3)') ODDSTD
        CALL CHR_CLEAN( CARD( 30) )
        LEN = 0
	CALL CHR_APPND( CARD( 30), CARD( 30), LEN)
	CARD( 30) = CARD( 30)( 1:LEN) // ' ' // CJUNK
	TEXT( 30) = ' / Odd channel std (DN)'

*      Even channel mean in statistics
	CARD( 31) = 'EVENMEAN='
	WRITE( CJUNK, '(F20.3)') EVENMEAN
        CALL CHR_CLEAN( CARD( 31) )
        LEN = 0
	CALL CHR_APPND( CARD( 31), CARD( 31), LEN)
	CARD( 31) = CARD( 31)( 1:LEN) // ' ' // CJUNK
	TEXT( 31) = ' / Even channel mean (DN)'

*      Even channel standard deviation in statistics
	CARD( 32) = 'EVENSTD ='
	WRITE( CJUNK, '(F20.3)') EVENSTD
        CALL CHR_CLEAN( CARD( 32) )
        LEN = 0
	CALL CHR_APPND( CARD( 32), CARD( 32), LEN)
	CARD( 32) = CARD( 32)( 1:LEN) // ' ' // CJUNK
	TEXT( 32) = ' / Even channel std (DN)'

*      Filter used for observations
	CARD( 33) = 'FILTER  ='
        CALL CHR_CLEAN( FILTER )
        LEN = 0
	CALL CHR_APPND( FILTER, FILTER, LEN)
	IF( LEN .LT. 8) THEN
	  FILTER( LEN+1:8) = ' '
	  LEN = 8
	END IF
	CJUNK = CHAR( 39) // FILTER( 1:LEN) // CHAR( 39)
        CALL CHR_CLEAN( CARD( 33) )
        LEN = 0
	CALL CHR_APPND( CARD( 33), CARD( 33), LEN)
	CARD( 33) = CARD( 33)( 1:LEN) // ' ' // CJUNK
	TEXT( 33) = ' / Filter selected'

*      Exposure time on chip
	CARD( 34) = 'EXPOSURE='
	WRITE( CJUNK, '(F20.3)') EXPO
        CALL CHR_CLEAN( CARD( 34) )
        LEN = 0
	CALL CHR_APPND( CARD( 34), CARD( 34), LEN)
	CARD( 34) = CARD( 34)( 1:LEN) // ' ' // CJUNK
	TEXT( 34) = ' / Exposure time in mSeconds'

*      Number of coadds in data
	CARD( 35) = 'NUMCOADD='
	WRITE( CJUNK, '(F20.3)') COADDS
        CALL CHR_CLEAN( CARD( 35) )
        LEN = 0
	CALL CHR_APPND( CARD( 35), CARD( 35), LEN)
	CARD( 35) = CARD( 35)( 1:LEN) // ' ' // CJUNK
	TEXT( 35) = ' / Number of coadds'

*      Effective integration time in data
	CARD( 36) = 'EFFITIME='
	WRITE( CJUNK, '(F20.3)') COADDS*EXPO/1000.0
        CALL CHR_CLEAN( CARD( 36) )
        LEN = 0
	CALL CHR_APPND( CARD( 36), CARD( 36), LEN)
	CARD( 36) = CARD( 36)( 1:LEN) // ' ' // CJUNK
	TEXT( 36) = ' / Effective integration time in Seconds'

*      Readout rate of array
	CARD( 37) = 'READRATE='
	WRITE( CJUNK, '(F20.3)') READRATE
        CALL CHR_CLEAN( CARD( 37) )
        LEN = 0
	CALL CHR_APPND( CARD( 37), CARD( 37), LEN)
	CARD( 37) = CARD( 37)( 1:LEN) // ' ' // CJUNK
	TEXT( 37) = ' / Readout time in mSeconds'

*      RA of telescope
	RRA = RA( 1)+RA( 2)/60.0+RA( 3)/3600.0
	CARD( 38) = 'REALRA  ='
	WRITE( CJUNK, '(F20.6)') RRA
	CARD( 38) = CARD( 38)( 1:LEN) // ' ' // CJUNK
	TEXT( 38) = ' / RA of observation in hours'

*      Declination of telescope
	IF( DEC( 1) .LT. 0.0) THEN
	  RDEC = DEC( 1)-DEC( 2)/60.0-DEC( 3)/3600.0
	ELSE
	  RDEC = DEC( 1)+DEC( 2)/60.0+DEC( 3)/3600.0
	END IF
	CARD( 39) = 'REALDEC ='
	WRITE( CJUNK, '(F20.6)') RDEC
        CALL CHR_CLEAN( CARD( 39) )
        LEN = 0
	CALL CHR_APPND( CARD( 39), CARD( 39), LEN)
	CARD( 39) = CARD( 39)( 1:LEN) // ' ' // CJUNK
	TEXT( 39) = ' / DEC of observation in degrees'

*      Equinox of telescope coordinates
	CARD( 40) = 'EQUINOX ='
	WRITE( CJUNK, '(F20.3)') EQUINOX
        CALL CHR_CLEAN( CARD( 40) )
        LEN = 0
	CALL CHR_APPND( CARD( 40), CARD( 40), LEN)
	CARD( 40) = CARD( 40)( 1:LEN) // ' ' // CJUNK
	TEXT( 40) = ' / Equinox of coordinates'

*      UT of observation
	RUT = UT( 1)+UT( 2)/60.0+UT( 3)/3600.0+UT( 4)/3600000.0
	CARD( 41) = 'REALUT  ='
	WRITE( CJUNK, '(F20.8)') RUT
        CALL CHR_CLEAN( CARD( 41) )
        LEN = 0
	CALL CHR_APPND( CARD( 41), CARD( 41), LEN)
	CARD( 41) = CARD( 41)( 1:LEN) // ' ' // CJUNK
	TEXT( 41) = ' / UT of observation in hours'

*      Hour angle of observation
	RHA = HA( 1)+HA( 2)/60.0+HA( 3)/3600.0
	CARD( 42) = 'REALHA  ='
	WRITE( CJUNK, '(F20.4)') RHA
        CALL CHR_CLEAN( CARD( 42) )
        LEN = 0
	CALL CHR_APPND( CARD( 42), CARD( 42), LEN)
	CARD( 42) = CARD( 42)( 1:LEN) // ' ' // CJUNK
	TEXT( 42) = ' / HA of observation in hours'

*      Siderial time of observation
	RST = ST( 1)+ST( 2)/60.0+ST( 3)/3600.0
	CARD( 43) = 'REALLST ='
	WRITE( CJUNK, '(F20.4)') RST
        CALL CHR_CLEAN( CARD( 43) )
        LEN = 0
	CALL CHR_APPND( CARD( 43), CARD( 43), LEN)
	CARD( 43) = CARD( 43)( 1:LEN) // ' ' // CJUNK
	TEXT( 43) = ' / LST of observation in hours'

*      Offset RA
	CARD( 44) = 'OFFSTRA ='
	WRITE( CJUNK, '(F20.3)') OFFSETRA
        CALL CHR_CLEAN( CARD( 44) )
        LEN = 0
	CALL CHR_APPND( CARD( 44), CARD( 44), LEN)
	CARD( 44) = CARD( 44)( 1:LEN) // ' ' // CJUNK
	TEXT( 44) = ' / RA offset in arcseconds'

*      Offset DEC
	CARD( 45) = 'OFFSTDEC='
	WRITE( CJUNK, '(F20.3)') OFFSETDEC
        CALL CHR_CLEAN( CARD( 45) )
        LEN = 0
	CALL CHR_APPND( CARD( 45), CARD( 45), LEN)
	CARD( 45) = CARD( 45)( 1:LEN) // ' ' // CJUNK
	TEXT( 45) = ' / DEC offset in arcseconds'

*      Local date/time
	CARD( 46) = 'LOCALTIM='
        CALL CHR_CLEAN( LOCALTIME )
        LEN = 0
	CALL CHR_APPND( LOCALTIME, LOCALTIME, LEN)
	IF( LEN .LT. 8) THEN
	  LOCALTIME( LEN+1:8) = ' '
	  LEN = 8
	END IF
	CJUNK = CHAR( 39) // LOCALTIME( 1:LEN) // CHAR( 39)
        CALL CHR_CLEAN( CARD( 46) )
        LEN = 0
	CALL CHR_APPND( CARD( 46), CARD( 46), LEN)
	CARD( 46) = CARD( 46)( 1:LEN) // ' ' // CJUNK
	TEXT( 46) = ' / Local time (VAX format)'

*      Date
	CARD( 47) = 'DATE-OBS='
	DATE1 = LOCALTIME( 1:2)
	DATE2 = LOCALTIME( 4:6)
	DATE3 = LOCALTIME( 10:11)
	IF( DATE2 .EQ. 'JAN') RDATE2 = '01'
	IF( DATE2 .EQ. 'FEB') RDATE2 = '02'
	IF( DATE2 .EQ. 'MAR') RDATE2 = '03'
	IF( DATE2 .EQ. 'APR') RDATE2 = '04'
	IF( DATE2 .EQ. 'MAY') RDATE2 = '05'
	IF( DATE2 .EQ. 'JUN') RDATE2 = '06'
	IF( DATE2 .EQ. 'JUL') RDATE2 = '07'
	IF( DATE2 .EQ. 'AUG') RDATE2 = '08'
	IF( DATE2 .EQ. 'SEP') RDATE2 = '09'
	IF( DATE2 .EQ. 'OCT') RDATE2 = '10'
	IF( DATE2 .EQ. 'NOV') RDATE2 = '11'
	IF( DATE2 .EQ. 'DEC') RDATE2 = '12'
	CJUNK =
     : CHAR( 39) // DATE1 // '/' // RDATE2 // '/' // DATE3 // CHAR( 39)
        CALL CHR_CLEAN( CARD( 47) )
        LEN = 0
	CALL CHR_APPND( CARD( 47), CARD( 47), LEN)
	CARD( 47) = CARD( 47)( 1:LEN) // ' ' // CJUNK
	TEXT( 47) = ' / Date of observations'

*      Time
	CARD( 48) = 'TIME-OBS='
	TIME = LOCALTIME( 13:20)
        CALL CHR_CLEAN( TIME )
        LEN = 0
	CALL CHR_APPND( TIME, TIME, LEN)
	IF( LEN .LT. 8) THEN
	  TIME( LEN+1:8) = ' '
	  LEN = 8
	END IF
	CJUNK = CHAR( 39) // TIME( 1:LEN) // CHAR( 39)
        CALL CHR_CLEAN( CARD( 48) )
        LEN = 0
	CALL CHR_APPND( CARD( 48), CARD( 48), LEN)
	CARD( 48) = CARD( 48)( 1:LEN) // ' ' // CJUNK
	TEXT( 48) = ' / Time of observation'

*      Temperature of array
	CARD( 49) = 'TEMPERAT='
	WRITE( CJUNK, '(F20.3)') TEMP
        CALL CHR_CLEAN( CARD( 49) )
        LEN = 0
	CALL CHR_APPND( CARD( 49), CARD( 49), LEN)
	CARD( 49) = CARD( 49)( 1:LEN) // ' ' // CJUNK
	TEXT( 49) = ' / Array temperature in K'

*      Airmass
	CARD( 50) = 'AIRMASS ='
	WRITE( CJUNK, '(F20.3)') AIRMASS
        CALL CHR_CLEAN( CARD( 50) )
        LEN = 0
	CALL CHR_APPND( CARD( 50), CARD( 50), LEN)
	CARD( 50) = CARD( 50)( 1:LEN) // ' ' // CJUNK
	TEXT( 50) = ' / Airmass of observation'

*      Name of image
	CARD( 51) = 'IMAGENAM='
        CALL CHR_CLEAN( IMAGENAME )
        LEN = 0
	CALL CHR_APPND( IMAGENAME, IMAGENAME, LEN)
	IF( LEN .LT. 8) THEN
	  IMAGENAME( LEN+1:8) = ' '
	  LEN = 8
	END IF
	CJUNK = CHAR( 39) // IMAGENAME( 1:LEN) // CHAR( 39)
        CALL CHR_CLEAN( CARD( 51) )
        LEN = 0
	CALL CHR_APPND( CARD( 51), CARD( 51), LEN)
	CARD( 51) = CARD( 51)( 1:LEN) // ' ' // CJUNK
	TEXT( 51) = ' / Image name'

*      Orientation
	CARD( 52) = 'ORIENTAT='
	CJUNK = CHAR( 39) // 'NORTH UP, EAST LEFT' // CHAR( 39)
        CALL CHR_CLEAN( CARD( 52) )
        LEN = 0
	CALL CHR_APPND( CARD( 52), CARD( 52), LEN)
	CARD( 52) = CARD( 52)( 1:LEN) // ' ' // CJUNK
	TEXT( 52) = ' / Orientation of image'

*      FP X
	CARD( 53) = 'FPX     ='
	WRITE( CJUNK, '(F20.3)') FPX
        CALL CHR_CLEAN( CARD( 53) )
        LEN = 0
	CALL CHR_APPND( CARD( 53), CARD( 53), LEN)
	CARD( 53) = CARD( 53)( 1:LEN) // ' ' // CJUNK
	TEXT( 53) = ' / FP X position'

*      FP Y
	CARD( 54) = 'FPY     ='
	WRITE( CJUNK, '(F20.3)') FPY
        CALL CHR_CLEAN( CARD( 54) )
        LEN = 0
	CALL CHR_APPND( CARD( 54), CARD( 54), LEN)
	CARD( 54) = CARD( 54)( 1:LEN) // ' ' // CJUNK
	TEXT( 54) = ' / FP Y position'

*      FP Z
	CARD( 55) = 'FPZ     ='
	WRITE( CJUNK, '(F20.3)') FPZ
        CALL CHR_CLEAN( CARD( 55) )
        LEN = 0
	CALL CHR_APPND( CARD( 55), CARD( 55), LEN)
	CARD( 55) = CARD( 55)( 1:LEN) // ' ' // CJUNK
	TEXT( 55) = ' / FP Z position'

*      XHEAD scale
	CARD( 56) = 'XSCALE  ='
	WRITE( CJUNK, '(F20.3)') XHEAD_ARCSECPMM
        CALL CHR_CLEAN( CARD( 56) )
        LEN = 0
	CALL CHR_APPND( CARD( 56), CARD( 56), LEN)
	CARD( 56) = CARD( 56)( 1:LEN) // ' ' // CJUNK
	TEXT( 56) = ' / Xhead scale arcsec/mm'

*      XHEAD DEC zero point
	CARD( 57) = 'XDECZERO='
	WRITE( CJUNK, '(F20.3)') XHEAD_DEC_ZERO
        CALL CHR_CLEAN( CARD( 57) )
        LEN = 0
	CALL CHR_APPND( CARD( 57), CARD( 57), LEN)
	CARD( 57) = CARD( 57)( 1:LEN) // ' ' // CJUNK
	TEXT( 57) = ' / Xhead DEC zero position'

*      XHEAD RA zero point
	CARD( 58) = 'XRAZERO ='
	WRITE( CJUNK, '(F20.3)') XHEAD_RA_ZERO
        CALL CHR_CLEAN( CARD( 58) )
        LEN = 0
	CALL CHR_APPND( CARD( 58), CARD( 58), LEN)
	CARD( 58) = CARD( 58)( 1:LEN) // ' ' // CJUNK
	TEXT( 58) = ' / Xhead RA zero position'

*      XHEAD DEC
	CARD( 59) = 'XDECPOSN='
	WRITE( CJUNK, '(F20.3)') XHEAD_DEC
        CALL CHR_CLEAN( CARD( 59) )
        LEN = 0
	CALL CHR_APPND( CARD( 59), CARD( 59), LEN)
	CARD( 59) = CARD( 59)( 1:LEN) // ' ' // CJUNK
	TEXT( 59) = ' / Xhead DEC position'

*      XHEAD RA
	CARD( 60) = 'XRAPOSN ='
	WRITE( CJUNK, '(F20.3)') XHEAD_RA
        CALL CHR_CLEAN( CARD( 60) )
        LEN = 0
	CALL CHR_APPND( CARD( 60), CARD( 60), LEN)
	CARD( 60) = CARD( 60)( 1:LEN) // ' ' // CJUNK
	TEXT( 60) = ' / Xhead RA position'

*      COMMENTS
	CARD( 61) = 'COMMENT1='
        CALL CHR_CLEAN( CARD( 61) )
        LEN = 0
	CALL CHR_APPND( CARD( 61), CARD( 61), LEN)
        CALL CHR_CLEAN( COMMENTS( 1) )
        LEN = 0
	CALL CHR_APPND( COMMENTS( 1), COMMENTS( 1), LEN2)
	CARD( 61) = CARD( 61)( 1:LEN) // ' ' // CHAR( 39) //
     :	  COMMENTS( 1)( 1:LEN2) // CHAR( 39)
	TEXT( 61) = ' '
	CARD( 62) = 'COMMENT2='
        CALL CHR_CLEAN( CARD( 62) )
        LEN = 0
	CALL CHR_APPND( CARD( 62), CARD( 62), LEN)
        CALL CHR_CLEAN( COMMENTS( 2) )
        LEN = 0
	CALL CHR_APPND( COMMENTS( 2), COMMENTS( 2), LEN2)
	CARD( 62) = CARD( 62)( 1:LEN) // ' ' // CHAR( 39) //
     :	  COMMENTS( 2)( 1:LEN2) // CHAR( 39)
	TEXT( 62) = ' '
	CARD( 63) = 'COMMENT3='
        CALL CHR_CLEAN( CARD( 63) )
        LEN = 0
	CALL CHR_APPND( CARD( 63), CARD( 63), LEN)
        CALL CHR_CLEAN( COMMENTS( 3) )
        LEN = 0
	CALL CHR_APPND( COMMENTS( 3), COMMENTS( 3), LEN2)
	CARD( 63) = CARD( 63)( 1:LEN) // ' ' // CHAR( 39) //
     :	  COMMENTS( 3)( 1:LEN2) // CHAR( 39)
	TEXT( 63) = ' '
	CARD( 64) = 'COMMENT4='
        CALL CHR_CLEAN( CARD( 64) )
        LEN = 0
	CALL CHR_APPND( CARD( 64), CARD( 64), LEN)
        CALL CHR_CLEAN( COMMENTS( 4) )
        LEN = 0
	CALL CHR_APPND( COMMENTS( 4), COMMENTS( 4), LEN2)
	CARD( 64) = CARD( 64)( 1:LEN) // ' ' // CHAR( 39) //
     :	  COMMENTS( 4)( 1:LEN2) // CHAR( 39)
	TEXT( 64) = ' '
	CARD( 65) = 'COMMENT5='
        CALL CHR_CLEAN( CARD( 65) )
        LEN = 0
	CALL CHR_APPND( CARD( 65), CARD( 65), LEN)
        CALL CHR_CLEAN( COMMENTS( 5) )
        LEN = 0
	CALL CHR_APPND( COMMENTS( 5), COMMENTS( 5), LEN2)
	CARD( 65) = CARD( 65)( 1:LEN) // ' ' // CHAR( 39) //
     :	  COMMENTS( 5)( 1:LEN2) // CHAR( 39)
	TEXT( 65) = ' '

*      End of fits header card
	CARD( 66) = 'END     '

*      Define number of the last card with END string
	ENDCARD = 66

*      Set index for NVALS array to 0
	NVALSINDEX = 0

*      Loop to scan through cards adding comment / to end of line and putting
*       characters into byte array for writing to tape
	DO JCARD = 1, ENDCARD

*        Test if the current card is less than or equal to the end card number
	  IF( JCARD .LT. ENDCARD) THEN

*          Put cards to upper case and trim to get its length
            CALL CHR_CLEAN( CARD( JCARD) )
            LEN = 0
	    CALL CHR_APPND( CARD( JCARD), CARD( JCARD), LEN)
            CALL CHR_CLEAN( TEXT( JCARD) )
            LEN = 0
	    CALL CHR_APPND( TEXT( JCARD), TEXT( JCARD), LEN2)

*          Add the comment / character
	    CARD( JCARD) = CARD( JCARD)( 1:LEN)//TEXT( JCARD)( 1:LEN2)
	    CALL CHR_UCASE( CARD( JCARD) )
	    LEN = LEN + LEN2
	    IF( LEN .GT. MAXCHARS) LEN =  MAXCHARS
	  END IF

	  write( 42, '(a)') card( jcard)( 1:len)

*        Scan through the card to put characters into byte array
	  DO JCHAR = 1, MAXCHARS

*          read characters from string to byte array
	    NVALSINDEX = NVALSINDEX + 1
	    IF( NVALSINDEX .LE. MAXNVALS) THEN
	      READ( CARD( JCARD)( JCHAR:JCHAR), '(A1)') NVALS( NVALSINDEX)
	    ELSE

*            Write header record to tape
	      IF( TAPEORDISK( 1:1) .EQ. 'T') THEN
	        JTAPE = MTWR( MTCHAN, NVALS, 1, 2880)
	      ELSE
	        WRITE( LUNO) NVALS
	      END IF

*            reset for next character from string to byte array
	      NVALSINDEX = 1
	      READ( CARD( JCARD)( JCHAR:JCHAR), '(A1)') NVALS( NVALSINDEX)
	    END IF
	  END DO
	END DO

*      Write header record to tape
	IF( TAPEORDISK( 1:1) .EQ. 'T') THEN
	  JTAPE = MTWR( MTCHAN, NVALS, 1, 2880)
	ELSE
	  WRITE( LUNO) NVALS
	END IF

	END
