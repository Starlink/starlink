      SUBROUTINE GETASTROM( IOUT, STATUS )
*+
*  Name:
*     GETASTROM

*  Purpose:
*     Create a file for input to the ASTROM program

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GETASTROM( IOUT, STATUS )

*  Description:
*     This subroutine does all the hard work in formatting a file based
*     on the intermediate file, as input to the ASTROM astrometry
*     program. Basically it asks for various parameters, picks some
*     reference stars, and then asks for some unknowns.
*     The information is written to logical unit IOUT.

*  Arguments:
*     IOUT = INTEGER (Given)
*        The I/O unit number of the output file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     CHR:
*        CHR_LEN, CHR_UCASE, CHR_CTOI, CHR_CTOR

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

*  Authors:
*     KFH: Ken Hartley (RGO)
*     SMB: Steven Beard (ROE)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     Sometime (SMB):
*        Pre-original version called GETASTROM
*     24-JAN-1983 (KFH):
*        Original version.
*     9-DEC-1991 (PMA):
*        Changed calls to CTOI to CHR_CTOI.
*        Changed calls to CTOR to CHR_CTOR.
*     11-JAN-1993 (PMA):
*        Converted to use the ADAM parameter system.
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CONST call
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CONV call
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*
*     11-MAR-1993 (AJJB):
*        Changed I, ISIGN1 and JSIGN (used as 4th argument in calls to
*        CONV) to type Character, as CONV has been changed.
*     15-MAR-1993 (AJJB):
*        Changed declaration of NOSCAL to type LOGICAL, and added a
*        line which sets it thus :
*        NOSCAL = SCALE.LT.1E-6
*        as it was declared integer, yet used as a logical, and never
*        set ! We assumed this was the right condition to set the flag,
*        as it's the only condition which sets this flag in the rest of
*        the package.
*     22-MAR-1993 (AJJB):
*        Commented out declarations of local variables which are never
*        used.
*     27-APR-1993 (AJJB):
*        Added calls to ERR_FLUSH at points where an invalid value has
*        been entered by user, so that STATUS gets restored to OK value
*        before prompting user again for value, as it was getting into
*        infinite loops.
*     17-MAY-1993 (AJJB):
*        Where parameter 'NUMBER' is read in, instead of checking
*        whether STATUS is set to a non-OK value as an indication that
*        the user pressed CR to exit the reading loop, it now checks
*        whether the value returned is a blank character.
*     18-MAY-1993 (AJJB):
*        In loops where things are read in and the user exits by
*        pressing <CR>, I changed the detection of <CR> so that it tests
*        for the parameter being set to a blank character. It used to
*        test for STATUS .NE. SAI__OK. I also removed the calls to ERR_REP
*        in the IF-block where the <CR> is detected at the points where X
*        and RA are read in.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'MAIN'             ! The CHART main common blocks
*        {descriptions_of_global_variables_referenced}...

      INCLUDE 'CONVF'            ! Constants used for conversions
*        {descriptions_of_global_variables_referenced}...

*  Arguments Given:
      INTEGER IOUT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! The length of a character string

*  Local Variables:
      CHARACTER * ( 50 ) VALUE   ! [local_variable_description]
      CHARACTER * ( 50 ) TEMP    ! [local_variable_description]
      CHARACTER * ( 50 ) RADEC
      CHARACTER * ( 50 ) TEMP1   ! [local_variable_description]
      CHARACTER * ( 50 ) EQCEN   ! [local_variable_description]
      CHARACTER * ( 50 ) EPOCHPL ! [local_variable_description]
      CHARACTER * ( 10 ) MAGCH   ! [local_variable_description]
*     INTEGER ISIGN              ! [local_variable_description]
      CHARACTER ISIGN1           ! [local_variable_description]
*     INTEGER ISIGN2( 1 )        ! [local_variable_description]
      DOUBLE PRECISION ACEN      ! [local_variable_description]
      DOUBLE PRECISION DCEN      ! [local_variable_description]
      DOUBLE PRECISION AVAL      ! [local_variable_description]
      DOUBLE PRECISION DVAL      ! [local_variable_description]
      CHARACTER * ( 1 ) UNKN     ! [local_variable_description]
      REAL E                     ! [local_variable_description]
      INTEGER L                  ! [local_variable_description]
      CHARACTER I                ! [local_variable_description]
      INTEGER IHR                ! [local_variable_description]
      INTEGER IMIN               ! [local_variable_description]
      INTEGER J                  ! [local_variable_description]
      REAL RSECS                 ! [local_variable_description]
      INTEGER IDEG               ! [local_variable_description]
      INTEGER IMIND              ! [local_variable_description]
      REAL DSECS                 ! [local_variable_description]
      REAL EQC                   ! [local_variable_description]
      REAL DATE                  ! [local_variable_description]
      REAL EPPL                  ! [local_variable_description]
      INTEGER LQ                 ! [local_variable_description]
      INTEGER LP                 ! [local_variable_description]
      INTEGER N                  ! [local_variable_description]
      DOUBLE PRECISION RAO       ! [local_variable_description]
      DOUBLE PRECISION DECO      ! [local_variable_description]
      REAL RRAO                  ! Takes RAO value to store as a real
      REAL RDECO                 ! Takes DECO value to store as a real
      INTEGER MHAO               ! [local_variable_description]
      INTEGER MINSAO             ! [local_variable_description]
      INTEGER NO                 ! [local_variable_description]
      REAL SECSAO                ! [local_variable_description]
      CHARACTER JSIGN            ! [local_variable_description]
      INTEGER MDEGD              ! [local_variable_description]
      INTEGER MINSD              ! [local_variable_description]
      REAL SECSD                 ! [local_variable_description]
      REAL RAPM                  ! [local_variable_description]
      REAL DECPM                 ! [local_variable_description]
      REAL RMAG                  ! [local_variable_description]
      REAL XVAL                  ! [local_variable_description]
      REAL YVAL                  ! [local_variable_description]
      LOGICAL NOSCAL             ! [local_variable_description]
      REAL XM                    ! [local_variable_description]
      REAL YM                    ! [local_variable_description]
      REAL X                     ! [local_variable_description]
      REAL Y                     ! [local_variable_description]
      REAL EQ                    ! [local_variable_description]
      REAL SECSR                 ! [local_variable_description]
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First check if decent positions are available.
      IF ( .NOT. CATRUN ) THEN
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT(' ', 'You are advised to use MODE=AST for this',
     :      STATUS )
         CALL MSG_OUT(' ', 'application, because proper motions are',
     :      STATUS )
         CALL MSG_OUT(' ', 'not available in the other catalogues',
     :      STATUS )
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT(' ', 'THEREFORE BEWARE OF THESE ANSWERS', STATUS )
         CALL MSG_BLANK( STATUS )
      END IF

*  Then get the equinox of the results.
*  (Default is the value used in SEARCH)
      WRITE ( VALUE, '(F10.1)' ) EQUOUT
  100 CONTINUE
      TEMP = VALUE
      CALL PAR_DEF0C( 'EQRES', VALUE, STATUS )
      CALL PAR_GET0C( 'EQRES', VALUE, STATUS )
      CALL CHR_UCASE( VALUE )

*  Test for presence of J or B
      IF ( VALUE( 1:1 ) .EQ. 'B' .OR. VALUE( 1:1 ) .EQ. 'J' ) THEN
         TEMP1 = VALUE( 2: )
      ELSE
         TEMP1 = VALUE
      ENDIF
      CALL CHR_CTOR( TEMP1, E, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         CALL PAR_CANCL( 'EQRES', STATUS )
         VALUE = TEMP
         GO TO 100
      END IF
      L = CHR_LEN( VALUE )
      WRITE ( IOUT, 900 ) VALUE( 1:L )
  900 FORMAT ( /, '* RESULTS EQUINOX :-', // A )

*  Now get the telescope type.
      VALUE = 'SCHMIDT'
  200 CONTINUE
      CALL PAR_DEF0C( 'TELTYPE', VALUE, STATUS )
      CALL PAR_GET0C( 'TELTYPE', VALUE, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL CHR_UCASE( VALUE )
      END IF
      IF ( VALUE( 1:4 ) .NE. 'ASTR' .AND. VALUE( 1:4 ) .NE. 'AAT2' .AND.
     :     VALUE( 1:4 ) .NE. 'AAT3' ) VALUE = 'SCHMIDT'
      WRITE ( IOUT, 910 ) VALUE
  910 FORMAT ( /, '* TELESCOPE TYPE :-', // A10 )

*  Then get RA,DEC of plate centre.
*  (Might not be the same as the centre used for Chart).
  300 CONTINUE
      VALUE = ' '
      CALL PAR_GET0C( 'RACEN', VALUE, STATUS )
      CALL CHR_UCASE( VALUE )
      CALL CONVRA( VALUE, 1, 50, ACEN, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PAR_CANCL( 'RACEN', STATUS )
         CALL ERR_FLUSH( STATUS )
         GO TO 300
      END IF

*  Now repeat for Declination
  310 CONTINUE
      VALUE = ' '
      CALL PAR_GET0C( 'DECEN', VALUE, STATUS )
      CALL CHR_UCASE( VALUE )
      CALL CONVDEC( VALUE, 1, 50, DCEN, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP('BAD_DEC', 'Invalid format for a DEC', STATUS )
         CALL ERR_FLUSH( STATUS )
         CALL PAR_CANCL( 'DECEN', STATUS)
         GO TO 310
      END IF

*  Now convert radians back into HMS,DMS.
      CALL CONV( 2, ACEN, 3, I, IHR, IMIN, J, RSECS , STATUS )
      CALL CONV( 1, DCEN, 2, ISIGN1, IDEG, IMIND, J, DSECS , STATUS )

*  and write them in a format acceptable to ASTROM

      WRITE ( RADEC, 915 ) IHR, IMIN, RSECS, ISIGN1, IDEG, IMIND, DSECS
  915 FORMAT( 2I3, F7.3, 3X, A1, 2I3, F6.2 )
      CALL CONST( ACEN, DCEN , STATUS )

*  and the equinox of this position.
      WRITE ( VALUE, '(F10.1)' ) EQUIN
      TEMP = VALUE
  400 CONTINUE
      CALL PAR_DEF0C( 'EQCENTRE', VALUE, STATUS )
      CALL PAR_GET0C( 'EQCENTRE', VALUE, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL CHR_UCASE( VALUE )
      END IF

*  Test for presence of J or B
      IF ( VALUE( 1:1 ) .EQ. 'B' .OR. VALUE( 1:1 ) .EQ. 'J' ) THEN
         TEMP1 = VALUE( 2: )
      ELSE
         TEMP1 = VALUE
      ENDIF
      CALL CHR_CTOR( TEMP1, EQC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'INVALID_EQUINOX', 'Invalid equinox given',
     :      STATUS )
         CALL PAR_CANCL( 'EQCENTRE', STATUS )
         VALUE = TEMP
         CALL ERR_FLUSH( STATUS )
         GO TO 400
      END IF
      EQCEN = VALUE

*  Now the date on which the plate was taken.
      CALL TODAY( DATE , STATUS )
      WRITE ( VALUE, '(F8.3)' ) DATE
      TEMP = VALUE
  500 CONTINUE
      CALL PAR_DEF0C( 'EPPLATE', VALUE, STATUS )
      CALL PAR_GET0C( 'EPPLATE', VALUE, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL CHR_UCASE( VALUE )
      END IF

*  Test for presence of J or B
      IF( VALUE( 1:1 ) .EQ. 'B' .OR. VALUE( 1:1 ) .EQ. 'J' ) THEN
         TEMP1 = VALUE( 2: )
      ELSE
         TEMP1 = VALUE
      ENDIF
      CALL CHR_CTOR( TEMP1, EPPL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'INVALID_EPOCH', 'This is not a valid epoch',
     :      STATUS )
         CALL ERR_FLUSH( STATUS )
         CALL PAR_CANCL( 'EPPLATE', STATUS )
         VALUE = TEMP
         GO TO 500
      END IF
      EPOCHPL = VALUE

*  We can now write the plate details to the file.
      LQ = CHR_LEN( EQCEN )
      LP = CHR_LEN( EPOCHPL )
      WRITE ( IOUT, 930 ) RADEC, EQCEN( 1:LQ ), EPOCHPL( 1:LP )
  930 FORMAT( /, '* PLATE DETAILS :-', // A30, 1X, A10, 1X, A10 )

*  Now handle reference stars.
      WRITE ( IOUT, 940 )
  940 FORMAT ( /, '* REFERENCE STARS :-' )

*  Loop around to here requesting numbers of stars in the list produced
*  by FORMAT.
  600 CONTINUE
      VALUE = ' '
      CALL PAR_GET0C( 'NUMBER', VALUE, STATUS )
      IF ( VALUE(:1) .EQ. ' ' ) GO TO 700
      CALL CHR_UCASE( VALUE )
      CALL CHR_CTOI( VALUE, N, STATUS )
      IF ( N .LE. 0 .OR. N .GT. NUM ) THEN
         STATUS = SAI__ERROR
      END IF
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'INVALID_ENTRY', 'Invalid entry for NUMBER',
     :      STATUS )
         CALL PAR_CANCL( 'NUMBER', STATUS )
         CALL ERR_FLUSH( STATUS )
         GO TO 600
      END IF

*  Now obtain the RA and DEC of this entry.
      RAO =  STAR( 1, IP( N ) )
      DECO = STAR( 2, IP( N ) )
      CALL CONV( 2, RAO, 4, I, MHAO, MINSAO, NO, SECSAO , STATUS )
      CALL CONV( 1, DECO, 3, JSIGN, MDEGD, MINSD, NO, SECSD , STATUS )
      WRITE ( RADEC, 950 ) MHAO, MINSAO, SECSAO, JSIGN, MDEGD, MINSD,
     :   SECSD
  950 FORMAT( 2I3, F7.3, 4X, A1, 2I3, F6.2 )

*  and a Proper Motion
      RAPM = PM( 2, IP( N ) )
      DECPM = PM( 1, IP( N ) )

*  Having got a nicely formatted RA and DEC seek an X,Y for this star.
      TEMP = 'RA and DEC are :' // RADEC
      CALL MSG_OUT( ' ', TEMP, STATUS )
      RMAG = REAL( NSTAR( 1, IP( N ) ) ) / 10.0
      WRITE ( MAGCH, '(F6.1)' ) RMAG
      TEMP = 'Magnitude is' // MAGCH
      CALL MSG_OUT( ' ', TEMP, STATUS )

* Put RAO and DECO into RRAO and RDECO for call to PROJ as it expects
* Real rather than double precision arguments. (AJJB 9-MAR-1993).

      RRAO = REAL( RAO )
      RDECO = REAL( DECO )
      CALL PROJ( 1, RAO, DECO, XVAL, YVAL , STATUS )
      XVAL = NINT( XVAL / RDSA )
      YVAL = NINT( YVAL / RDSA )
      NOSCAL = SCALE.LT.1E-6
      IF ( .NOT. NOSCAL ) THEN
         XM = -( XVAL / SCALE )
         YM = YVAL / SCALE
         WRITE ( TEMP, 951 ) XM, YM
  951    FORMAT( 'X is about ', F7.2, ' and Y is about ', F7.2,
     :      ' (mm.)' )
         CALL MSG_OUT( ' ', TEMP, STATUS )
      END IF
  610 CONTINUE

*  First the X co-ordinate
      VALUE = ' '
      CALL PAR_GET0C( 'X', VALUE, STATUS )
      CALL CHR_UCASE( VALUE )
      CALL CHR_CTOR( VALUE, XVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_OUT( ' ',  'INVALID_XCOORD Invalid X co-ordinate',
     :      STATUS )
         CALL PAR_CANCL( 'X', STATUS )
         CALL ERR_FLUSH( STATUS )
         GO TO 610
      END IF
      TEMP = VALUE
  620 CONTINUE

*  and then the Y co-ordinate
      CALL PAR_GET0C( 'Y', VALUE, STATUS )
      CALL CHR_UCASE( VALUE )
      CALL CHR_CTOR( VALUE, YVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( 'INVALID_YCOORD', 'Invalid Y co-ordinate',
     :     STATUS )
         CALL PAR_CANCL( 'Y', STATUS )
         CALL ERR_FLUSH( STATUS )
         GO TO 620
      END IF

*  At last write the two records to the output file.
      WRITE ( IOUT, 960 ) RADEC( 1:30 ), RAPM, DECPM, EQUOUT, EPOCH
  960 FORMAT( /, A30, 2F12.6, 2F10.3 )
      WRITE ( IOUT, 965 ) TEMP, VALUE, N
  965 FORMAT ( 2A15, '*Star ', I4 )

*  Go back for another star.
      CALL PAR_CANCL( 'NUMBER', STATUS )
      CALL PAR_CANCL( 'X', STATUS )
      CALL PAR_CANCL( 'Y', STATUS )
      GO TO 600

*  Come here after entering reference stars.
  700 CONTINUE

*  Now write the "unknown" records.
      WRITE ( IOUT, 970 )
  970 FORMAT( /, '* UNKNOWN STARS :-' )

*  Loop back to here after each entry.
  701 CONTINUE
      VALUE = ' '
      CALL PAR_GET0C( 'XORR', VALUE, STATUS )
      IF ( VALUE(:1) .EQ. ' ' ) GO TO 800
      CALL CHR_UCASE( VALUE )

*  Set one character into UNKN
      IF ( VALUE( 1:1 ) .EQ. 'X' ) THEN
         UNKN = 'X'
      ELSE
         UNKN = 'R'
      END IF

*  Now pick up the required information depending on the value of UNKN
      IF ( UNKN .EQ. 'X' ) THEN

*  Pick up an X and a Y
  710    CONTINUE
         VALUE = ' '
         CALL PAR_GET0C( 'X', VALUE, STATUS )
         IF ( VALUE(:1) .EQ. ' ' ) THEN
            CALL PAR_CANCL( 'X', STATUS )
            CALL PAR_CANCL( 'XORR', STATUS )
            GO TO 701
         ENDIF
         CALL CHR_UCASE( VALUE )
         CALL CHR_CTOR( VALUE, X, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'INVALID_XCOORD',
     :         'Invalid entry for x co-ordinate', STATUS )
            CALL PAR_CANCL( 'X', STATUS )
            CALL ERR_FLUSH( STATUS )
            GO TO 710
         END IF
  720    CONTINUE
         VALUE = ' '
         CALL PAR_GET0C( 'Y', VALUE, STATUS )
         CALL CHR_UCASE( VALUE )
         CALL CHR_CTOR( VALUE, Y, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'INVALID_YCOORD',
     :         'Invalid format for y co-ordinate', STATUS )
            CALL PAR_CANCL( 'Y', STATUS )
            CALL ERR_FLUSH( STATUS )
            GO TO 720
         END IF

*  Now store them as characters
         WRITE ( TEMP, 975 ) X, Y
  975    FORMAT ( 2F15.4 )
         CALL PAR_CANCL( 'X', STATUS )
         CALL PAR_CANCL( 'Y', STATUS )
  725    CONTINUE
         VALUE = ' '
         CALL PAR_GET0C( 'NAME', VALUE, STATUS )
         CALL CHR_UCASE( VALUE )
         CALL PAR_CANCL( 'NAME', STATUS )

*  Now write the full record to disk
         WRITE ( IOUT, 980 ) TEMP, VALUE( 1:10 )
  980    FORMAT ( /, A, 9X, '* ', A10 )
         GO TO 710

*  This completes the X,Y processing. A name is added later
      ELSE

*  Now handle the RA,DEC option
  730    CONTINUE
         VALUE = ' '
         CALL PAR_GET0C( 'RA', VALUE, STATUS )
         IF ( VALUE(:1) .EQ. ' ' ) THEN
            CALL PAR_CANCL( 'RA', STATUS )
            CALL PAR_CANCL( 'XORR', STATUS )
            GO TO 701
         ENDIF
         CALL CHR_UCASE( VALUE )
         CALL CONVRA( VALUE, 1, 50, AVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PAR_CANCL( 'RA', STATUS )
            CALL ERR_FLUSH( STATUS )
            GO TO 730
         END IF
  740    CONTINUE
         VALUE = ' '
         CALL PAR_GET0C( 'DEC', VALUE, STATUS )
         CALL CHR_UCASE( VALUE )
         CALL CONVDEC( VALUE, 1, 50, DVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PAR_CANCL( 'DEC', STATUS )
            CALL ERR_FLUSH( STATUS )
            GO TO 740
         END IF

*  In this case we also need an equinox
  750    CONTINUE
         WRITE ( VALUE, '(F9.3)' ) EQC
         CALL PAR_DEF0C( 'EQUINOX', VALUE, STATUS )
         CALL PAR_GET0C( 'EQUINOX', VALUE, STATUS )
         CALL CHR_UCASE( VALUE )

*  TODAY is a valid response
         IF ( VALUE( 1:3 ) .EQ. 'TOD' ) THEN
            CALL TODAY( DATE , STATUS )
            WRITE ( VALUE, '(F10.3)' ) DATE
         END IF
         CALL CHR_CTOR( VALUE, EQ, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'INVALID_EQUINOX',
     :         'Invalid format for an Equinox', STATUS)
            CALL PAR_CANCL( 'EQUINOX', STATUS )
            CALL ERR_FLUSH( STATUS )
            GO TO 750
         END IF

*  Now convert the radians back into hms and dms
         CALL CONV( 2, AVAL, 3, I, IHR, IMIN, J, SECSR , STATUS )
         CALL CONV( 1, DVAL, 2, ISIGN1, IDEG, IMIND, J, SECSD , STATUS )

*  and make up a formatted entry
          WRITE ( TEMP, 976 ) IHR, IMIN, SECSR, ISIGN1, IDEG, IMIND,
     :       SECSD, EQ
  976     FORMAT( 2I3, F7.3, 4X, A1, 2I3, F6.2, 4X, F8.3 )

*  This completes the RA,DEC option
         CALL PAR_CANCL( 'RA', STATUS )
         CALL PAR_CANCL( 'DEC', STATUS )
         CALL PAR_CANCL( 'EQUINOX', STATUS )

*  In either case a NAME may also be supplied.
  760    CONTINUE
         VALUE = ' '
         CALL PAR_GET0C( 'NAME', VALUE, STATUS )
         CALL CHR_UCASE( VALUE )
         CALL PAR_CANCL( 'NAME', STATUS )

*  Now write the full record to disk
         WRITE ( IOUT, 980 ) TEMP, VALUE( 1:10 )
         GO TO 730
      ENDIF

*  Come here at end of "unknown" entry.
  800 CONTINUE
      CALL PAR_CANCL( 'EQRES', STATUS )
      CALL PAR_CANCL( 'TELTYPE', STATUS )
      CALL PAR_CANCL( 'RACEN', STATUS )
      CALL PAR_CANCL( 'DECEN', STATUS )
      CALL PAR_CANCL( 'EQCENTRE', STATUS )
      CALL PAR_CANCL( 'EPPLATE', STATUS )
      CALL PAR_CANCL( 'NUMBER', STATUS )
      CALL PAR_CANCL( 'X', STATUS )
      CALL PAR_CANCL( 'Y', STATUS )
      CALL PAR_CANCL( 'XORR', STATUS )
      CALL PAR_CANCL( 'RA', STATUS )
      CALL PAR_CANCL( 'DEC', STATUS )
      CALL PAR_CANCL( 'EQUINOX', STATUS )
      CALL PAR_CANCL( 'NAME', STATUS )

      END
