*+UTIL_TDECODE - Decodes a range of times into offset format
      SUBROUTINE UTIL_TDECODE(TSTRING, BASE_MJD, MXRNG, NTRANGE,
     &                                    START, STOP, STATUS)
*    Description :
*     Takes a string containing a series of time ranges and decodes then
*     into a series of on-off times, where the output times are offsets
*     from a base time.
*
*     Input format :
*
*        a) A series of offset times - no decoding needed
*
*        b) A series of MJDs. Format is M479862.108 M479862.203
*
*        c) A file containing either offset times or MJDs, specified
*           by simply the filename.
*
*    Method :
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     Richard Saxton  (LTVAD::RDS)
*    History :
*     12-Jul-1993 Updated the number of allowed words in the
*                                          twords array to 4000 (RDS)
*     date:  changes (institution::username)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*     <any INCLUDE files containing global constant definitions>
*    Global variables :
*     <global variables held in named COMMON>
*    Structure definitions :
*     <specification of FORTRAN structures>
*    Import :
      CHARACTER*(*) TSTRING             ! String specifying the time range
      DOUBLE PRECISION BASE_MJD         ! Base MJD of the observation
      INTEGER MXRNG                     ! Max no. of time ranges
*    Import-Export :
*     <declarations and descriptions for imported/exported arguments>
*    Export :
      INTEGER NTRANGE                   ! Number of time ranges specified.
      REAL START(MXRNG)                 ! Start times - offset from base zero
*                                       !  in seconds.
      REAL STOP(MXRNG)                  ! Stop times - offset from base zero
*                                       !  in seconds.
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
      INTEGER CHR_ISALM,CHR_ISALF
        EXTERNAL CHR_ISALM,CHR_ISALF
*    Local constants :
      INTEGER MAXWRD
         PARAMETER (MAXWRD=4000)
*    Local variables :
      LOGICAL JUMPOUT
      INTEGER LP
      INTEGER LUNIT                        ! Logical unit for times file
      CHARACTER FCHAR1,FCHAR2
      CHARACTER*80 FNAME                   ! Name of file containing times
      CHARACTER*80 DUMSTRING               ! Dummy string variable
      INTEGER NTOT,NWRD
      INTEGER IDUM1(100),IDUM2(100)
      CHARACTER*30 WORDS(100)
      INTEGER ISTAT,K1,K2,WTOT
      CHARACTER*30 TWORD(MAXWRD)           ! Start and stop times
      DOUBLE PRECISION MJD                 ! Time as an MJD
      INTEGER IDOT                         ! Position of full stop
      INTEGER FERR
*    Local data :
*     <any DATA initialisations for local variables>
*-
* Find first alpha numeric character in the string
      JUMPOUT = .FALSE.
      LP=0
      NTRANGE = 0
*
      DO WHILE (.NOT. JUMPOUT)
*
         LP = LP + 1
         JUMPOUT = CHR_ISALM(TSTRING(LP:LP)).OR.
     :             (INDEX('[./',TSTRING(LP:LP)).NE.0)
*
      ENDDO
*
* Set the first meaningful character in string value
      FCHAR1 = TSTRING(LP:LP)
      FCHAR2 = TSTRING(LP+1:LP+1)
      IDOT = INDEX(TSTRING, '.')
*
* Are the time ranges contained in a file ? A file in this context has
* its first character as a letter or underscore and a second letter as
* a letter,underscore,slash or dot, or if it has an extension.
      IF ( ( (FCHAR1 .EQ. '_' .OR. CHR_ISALF(FCHAR1)) .AND.
     &     (INDEX('_./',FCHAR2) .NE. 0 .OR. CHR_ISALF(FCHAR2)) ) .OR.
     &     ( CHR_ISALF(TSTRING(IDOT+1:IDOT+1)) .AND.
     &      CHR_ISALF(TSTRING(IDOT+2:IDOT+2)) )) THEN
*
         FNAME = TSTRING(LP:CHR_LEN(TSTRING))
*
*   Open file
         CALL FIO_GUNIT(LUNIT, STATUS)
*
         OPEN(LUNIT, FILE=FNAME, STATUS='OLD', IOSTAT=FERR)
	 IF (FERR.NE.0) THEN
            CALL FIO_PUNIT(LUNIT, STATUS)
	    STATUS = SAI__ERROR
	    GOTO 999
         ENDIF
*
*   Read in each record as a series of words - which can be interpreted later
         JUMPOUT = .FALSE.
         WTOT = 0
*
         DO WHILE (.NOT. JUMPOUT)
*
            READ(LUNIT,'(A80)',END=100)DUMSTRING
*
            CALL CHR_DCWRD(DUMSTRING, MAXWRD, NWRD, IDUM1, IDUM2,
     &                     WORDS, ISTAT)
*
*   Write words into the big tword array
            DO LP=1,NWRD
               WTOT = WTOT + 1
               TWORD(WTOT) = WORDS(LP)
            ENDDO
*
         ENDDO
*
100      CONTINUE
*
*   Close the file
         CLOSE(LUNIT)
         CALL FIO_PUNIT(LUNIT, STATUS)
*
      ELSE
*
* If not a filename decode the input string directly into a series of
* words
         CALL CHR_DCWRD(TSTRING, MAXWRD, WTOT, IDUM1, IDUM2,
     &                  TWORD, ISTAT)
*
      ENDIF
*
* Check if
* Convert words into start and stop times
*  Check if the first word is an MJD definition - NB: strings with mixed
*  MJD and offset time definitions are not allowed.
      IF (INDEX(TWORD(1), 'M') .NE. 0) THEN
*
*    Check if word contains a delimiter character - ':'
         IF (INDEX(TWORD(1), ':') .NE. 0) THEN
*
*      Decode each word into a start and stop time
            DO LP=1,WTOT
*
               K1=INDEX(TWORD(LP),'M')
               K2=INDEX(TWORD(LP),':')
*
*         Decode the start time
               READ(TWORD(LP)(K1+1:K2-1),*)MJD
               START(LP) = ( MJD - BASE_MJD ) * 86400.
*
*         Decode the stop time
               READ(TWORD(LP)(K2+2:),*)MJD
               STOP(LP) = ( MJD - BASE_MJD ) * 86400.
*
            ENDDO
*
            NTRANGE = WTOT
*
         ELSE
*
*       Series of start stop MJDs to decode
            DO LP=1,WTOT,2
*
               NTRANGE = NTRANGE + 1
*
*         Decode the start time
               K1=INDEX(TWORD(LP),'M')
               READ(TWORD(LP)(K1+1:),*) MJD
               START(NTRANGE) = ( MJD - BASE_MJD ) * 86400.
*
*         Decode the stop time
               K1=INDEX(TWORD(LP+1),'M')
               READ(TWORD(LP+1)(K1+1:),*) MJD
               STOP(NTRANGE) = ( MJD - BASE_MJD ) * 86400.
*
            ENDDO
*
         ENDIF
*
      ELSE
*
* Not an MJD - i.e. offset times
*    Check if word contains a delimiter character - ':'
         IF (INDEX(TWORD(1), ':') .NE. 0) THEN
*
            DO LP=1,WTOT
*
               K1=INDEX(TWORD(LP),':')
*
               READ(TWORD(LP)(1:K1-1),*)START(LP)
               READ(TWORD(LP)(K1+1:),*)STOP(LP)
*
            ENDDO
*
            NTRANGE = WTOT
*
         ELSE
*
*     Decode each time individually
            DO LP=1,WTOT,2
*
               NTRANGE = NTRANGE + 1
*
               READ(TWORD(LP),*)START(NTRANGE)
               READ(TWORD(LP+1),*)STOP(NTRANGE)
*
            ENDDO
         ENDIF
      ENDIF
*
* Check if max ranges has been exceeded
      IF (NTRANGE .GT. MXRNG) THEN
         CALL MSG_SETI('MAX', MXRNG)
         CALL MSG_PRNT('** Maximum number of time ranges exceeded '/
     &                /'- only ^MAX may be used **')
         STATUS = SAI__ERROR
      ENDIF
999   CONTINUE
*
      END
