      SUBROUTINE SWITCH_DATE( IFAIL )
*+
*  Name:
*     SWITCH_DATE

*  Purpose:
*     Change a date part of the DATADIR environment variable and/or
*     a GSD file prefix.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SWITCH_DATE( IFAIL )

*  Description:
*     This routine prompts the user for a new date and changes
*     a corresponding date part in the DATADIR environment variable
*     and the GSD file prefix.

*  Arguments:
*     IFAIL = INTEGER (Returned)
*        The global status. The status is reset on entry.

*  Authors:
*     rpt: Remo Tilanus (JAC, Hilo)

*  History:
*     18 Mar 2003 (rpt)
*        First version

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      CHARACTER * ( 10 ) ENVVAR ! Name of DATA DIR envionment variable
      PARAMETER ( ENVVAR = 'DATADIR' )

*  External Functions:
      INTEGER GEN_ILEN

*  Global Variables:
      INCLUDE 'PSX_ERR'
      INCLUDE 'SAE_PAR'
      INCLUDE 'FLAGCOMM'         ! GSD Prefix: GSDNAME

*  Status:
      INTEGER IFAIL              ! Global status
      INTEGER STATUS             ! Starlink status

*  Local Variables:
*
      CHARACTER *( 132 ) CURDIR  ! Current data dir
      CHARACTER *( 132 ) NEWDIR  ! New Data dir
      CHARACTER *( 132 ) CURPRE  ! Current GSD file prefix
      CHARACTER *( 132 ) NEWPRE  ! new GSD file prefix
      CHARACTER *(   8 ) NEWDATE ! New date, either (YY)YYMMDD
      CHARACTER *(   8 ) YYYYMMDD ! New date, YYYYMMDD
      CHARACTER        MYCHAR

      INTEGER DATELEN            ! Length of CURDATE string
      INTEGER DIRLEN             ! Length of CURDIR  string
      INTEGER JDEF               ! Returned by GEN_GETSTR


*     We Seem to reset status on entry
      IFAIL = 0
      STATUS = SAI__OK

      CALL ERR_BEGIN( STATUS )

*     Prompt for the new date string
*     Prompt for string
      CALL GEN_GETSTR( 'New date? (YY)YYMMDD', '', '', NEWDATE, JDEF)

*     Only proceed if we got the value okay
      IF ( STATUS .EQ. SAI__OK .AND. JDEF .EQ. 0) THEN

*         MYCHAR = '?'
*         IF ( GEN_ILEN(NEWDATE) .GT. 0 ) THEN
*           MYCHAR = NEWDATE(1:1)
*         ENDIF
*
*     Give help if unexpected input
*         IF ( ICHAR(MYCHAR) .LT. ICHAR('0') .OR.
*     &        ICHAR(MYCHAR) .GT. ICHAR('9') ) THEN
*            print *,'NOTE: Date format must be YYYYMMDD or YYMMDD.'
*            print *,''
*            print *,'SWITCH-DATE will replace any date-like string in'
*            print *,'the DATADIR environment variable (SET-DATA-DIR)'
*            print *,'and the GSD prefix (SET-GSD-FILE) with the new'
*            print *,'date specified. It is a short-cut for running'
*            print *,'the commands mentioned, but those can be used'
*            print *,'instead or to fix any unexpected errors.'
*            print *,''
*         ENDIF

         DATELEN = GEN_ILEN( NEWDATE )
         IF ( DATELEN .EQ. 6 .OR. DATELEN .EQ. 8 ) THEN

            YYYYMMDD = NEWDATE
            IF ( DATELEN .EQ. 6 ) THEN
               YYYYMMDD(1:2) = '20'
               YYYYMMDD(3:8) = NEWDATE(1:6)
            ENDIF

*           Current prefix from include block
            CURPRE = GSDNAME

            CALL REPLACE_DATE ( YYYYMMDD, CURPRE, NEWPRE )

            IF (NEWPRE .NE. CURPRE) THEN
               GSDNAME = NEWPRE
               print *,'GSD PREFIX now set to: "',
     &                     NEWPRE(:GEN_ILEN(NEWPRE)),'"'
            ELSE
               print *,'GSD PREFIX not modified, still "',
     &                     CURPRE(:GEN_ILEN(CURPRE)),'"'
            ENDIF

*           Get the current DATADIR to show the default
            DIRLEN = 0
            CALL UTRNLOG( ENVVAR, CURDIR, STATUS )

*           Only if we got the value okay
            IF ( STATUS .EQ. SAI__OK .AND. STATUS .NE. PSX__NOENV ) THEN
               DIRLEN = GEN_ILEN( CURDIR )
               CALL REPLACE_DATE ( YYYYMMDD, CURDIR, NEWDIR )
            ELSEIF ( STATUS .EQ. PSX__NOENV ) THEN
               print *,'No DATADIR environment variable to modify.'
               CALL ERR_ANNUL( STATUS )
            ELSE
               print *,'Error reading DATADIR environment variable.'
               IFAIL = 18
               CALL ERR_ANNUL( STATUS )
            ENDIF

*           Set the new DATADIR value if modified
            IF (DIRLEN .NE. 0 .AND. NEWDIR .NE. CURDIR) THEN
               CALL UPUTLOG( ENVVAR, NEWDIR, STATUS )

               IF (STATUS .NE. SAI__OK ) THEN
                  IFAIL = 18
                  CALL ERR_ANNUL( STATUS )
               ENDIF

            ELSEIF ( DIRLEN .NE. 0 ) THEN

               print *,'Data directory not modified, still "',
     &                 CURDIR(:DIRLEN),'"'

            END IF

         ELSE

            print *,'Date format must be YYYYMMDD or YYMMDD.'
            IFAIL = 18
            CALL ERR_ANNUL( STATUS )

         ENDIF

      ENDIF


*     Set IFAIL to bad if status is bad
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_FLUSH( STATUS )
         IFAIL = 18
      END IF

      CALL ERR_END( STATUS )

      RETURN
      END


      SUBROUTINE REPLACE_DATE (YYYYMMDD, INSTRING, OUTSTRING)
*     Replaces any 'yyyymmdd' or 'yymmdd' like string in INSTRING.
*     with (YY)YYMMDD. If embedded completely, the date string searched
*     for is assumed to be enclosed by '/' or '_' characters.
*     OUTSTING is returns the modified string with the new date.

      CHARACTER*(  8)  YYYYMMDD
      CHARACTER*(  *)  INSTRING, OUTSTRING

*  External Functions:
      INTEGER GEN_ILEN

      INTEGER          STRLEN    ! Length INSTRING
      INTEGER          FOUND     ! Number of digits in current string
      CHARACTER*(132)  TMPSTR
      CHARACTER*(  6)  YYMMDD
      CHARACTER        CURCHAR

      YYMMDD = YYYYMMDD(3:8)

      STRLEN = GEN_ILEN(INSTRING)

      OUTSTRING = INSTRING
      TMPSTR = ''
      FOUND = 0
      ISTART = 0
      I = 1

*     Search through the strings for date-like parts enclosed
*     between '/' or '_' (or leading or terminating):
      DO WHILE ( I .LE. STRLEN )

        CURCHAR = INSTRING(I:I)

*       Found delimiter or end relevant part string
        IF ( CURCHAR .EQ. '/' .OR. CURCHAR .EQ. '_'
     &       .OR. CURCHAR .EQ. ' ' .OR. I .EQ. STRLEN ) THEN

*         Terminating character is number
          IF ( ICHAR(CURCHAR) .GE. ICHAR('0') .AND.
     &         ICHAR(CURCHAR) .LE. ICHAR('9') ) THEN
            FOUND = FOUND + 1
            TMPSTR(FOUND:FOUND) = CURCHAR
          ENDIF

*         Do poor man's sanity check on number:
*            month between 1 and 12, day between 1 and 31
          IF ( FOUND .GE. 6 ) THEN
            READ(TMPSTR(3:4),*) IMON
            READ(TMPSTR(5:6),*) IDAY
          ELSEIF ( FOUND .GE. 8 ) THEN
            READ(TMPSTR(5:6),*) IMON
            READ(TMPSTR(7:8),*) IDAY
          ELSE
            IMON = -1
            IDAY = -1
          ENDIF

*         Short date format found okay month and day: replace
          IF ( FOUND .EQ. 6
     &        .AND. IMON .GE. 1 .AND. IMON .LE. 12
     &        .AND. IDAY .GE. 1 .AND. IDAY .LE. 31 ) THEN
             OUTSTRING(ISTART:ISTART+5) = YYMMDD

*         Long date format found with okay month and day: replace
          ELSEIF ( FOUND .EQ. 8
     &        .AND. IMON .GE. 1 .AND. IMON .LE. 12
     &        .AND. IDAY .GE. 1 .AND. IDAY .LE. 31 ) THEN
             OUTSTRING(ISTART:ISTART+7) = YYYYMMDD
          ENDIF

          TMPSTR = ''
          FOUND = 0
          ISTART = 0

        ELSEIF ( ICHAR(CURCHAR) .GE. ICHAR('0')
     &           .AND. ICHAR(CURCHAR) .LE. ICHAR('9') ) THEN

          IF (FOUND .EQ. 0) THEN
            ISTART = I
          ENDIF
          FOUND = FOUND + 1
          TMPSTR(FOUND:FOUND) = CURCHAR

        ELSE

          TMPSTR = ''
          FOUND = 0
          ISTART = 0

        ENDIF

*       Force termination loop after first blank has been handled
        IF (CURCHAR .EQ. ' ') THEN
          I = STRLEN
        ENDIF

        I = I + 1

      ENDDO

      RETURN
      END
