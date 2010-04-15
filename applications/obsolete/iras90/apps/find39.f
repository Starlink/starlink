      SUBROUTINE FIND39( DISMOD, FD, PAGLEN, MAXLEN, SOBOT, SURVEY,
     : STATUS )
*+
*  Name:
*     FIND39

*  Purpose:
*     Either:-
*     To display to screen a single source or a page of sources from
*     all the sources input to FINDCRDD.
*     Or
*     To prepare a file of source details

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND39( DISMOD, FD, PAGLEN, MAXLEN, SOBOT, SURVEY, STATUS )

*  Description:
*     Either
*
*     To display to a screen a sub list from all the sources input to
*     FINDCRDD. This may be either a single line, indicated by PAGLEN
*     set to 1, or a list of several lines.
*
*     If a single line is required then PAGLEN will be 1, and SOBOT
*     will indicate the source position in source common that is to
*     be displayed. A single line will be displayed without a heading.
*
*     If a page is required, then PAGLEN will be greater than 1 and
*     SOBOT will be the bottom source required on the page, and the
*     first source required is calculated as SOBOT - PAGLEN + 1.
*     The page will be displayed with a two line heading.
*
*     Normally the PAGLEN will be less than or equal to the MAXLEN
*     page length In this case if there are not enough sources the
*     subroutine will put up those available and issue enough line
*     feeds to bring it to the bottom of the page.
*
*     However the program will not display any source who's
*     source-to-be-deleted marker is set .TRUE.. This will occur
*     during source deletion. So at this point the PAGLEN may be set
*     longer than MAXLEN to make a full page allowing for the deletions.
*
*     Or
*
*     The program send to a file details of all sources up to and
*     including the last entered as SOBOT. The program will not display
*     any source whos source-to-be-deleted marker is set .TRUE.. This
*     file can then be listed by the user.
*     Note This is not the file used to store source details for
*     reinput. That file contains further details such as title and
*     coords in RA and Dec radians (1950).
*
*     In both cases
*
*     The parameter SURVEY determins whether the region size and
*     wavebands will be displayed ( SURVEY = .TRUE.) or not.

*  Arguments:
*     DISMOD = LOGICAL (Given)
*        Display mode .TRUE. equals terminal, .FALSE. equals to file
*     FD     = INTEGER (Given)
*        File descriptor obtained from FIO in FIND28, or a dummy from
*        other subroutines
*     PAGLEN = INTEGER (Given)
*        Number of sources to be listed
*     MAXLEN = INTEGER
*        Number of lines per page on display
*     SOBOT  = INTEGER (Given)
*        Bottom source to be listed
*     SURVEY = LOGICAL (Given)
*        If the region size and wavebands are to be printed this is set
*        .TRUE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     FINDCRDD:
*        FIND10
*     ERR:
*        ERR_REP
*     FIO:
*        FIO_WRITE
*     MSG:
*        MSG_FMTC, MSG_FMTL, MSG_FMTR, MSG_OUT

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     23-APR-1992 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_PAR'          ! IRAS 90 General constants
      INCLUDE 'IRA_PAR'          ! IRAS Astrometry constants
      INCLUDE 'IRA_ERR'          ! IRAS Astrometry errors
      INCLUDE 'MSG_PAR'          ! Message reporting constants
      INCLUDE 'MSG_ERR'          ! Message reporting errors
      INCLUDE 'ERR_PAR'          ! Error reporting constants
      INCLUDE 'ERR_ERR'          ! Error reporting errors
      INCLUDE 'PAR_ERR'          ! Parameter errors

*  Global Variables:
      INCLUDE 'FICOMN' ! Common blocks for FINDCRDD

*  Arguments Given:
      LOGICAL DISMOD
      INTEGER FD
      INTEGER PAGLEN
      INTEGER MAXLEN
      INTEGER SOBOT
      LOGICAL SURVEY
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 80 ) BUFFER( 3 ) ! Buffer for output of source file
      CHARACTER * ( 1 ) CARRET   ! Contains the carrage control for
                                 ! internal writes/reads
      REAL DECROS                ! Cross scan size in arc minutes from
                                 ! individual source
      REAL DEINSC                ! In scan size in arc minutes from
                                 ! individual source
      INTEGER II                 ! DO loop control variable
      INTEGER III                ! DO loop control variable
      INTEGER IOS                ! Status of internal read/write
      INTEGER NOLINE             ! Count of number of lines displayed
      INTEGER SOTOP              ! First source to be displayed
      CHARACTER * ( 16 ) STNAME  ! Start of display line containing name
                                 ! and coordinate system (to stop MSG
                                 ! truncating the source name)
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IOS = 0
      NOLINE = 0

*  *********************************************************************
*  Check whether display is to terminal ( DISMOD = .TRUE. ) or
*  to file ( DISMOD = .FALSE. )
*  *********************************************************************
      IF ( DISMOD ) THEN


*  *********************************************************************
*  *********************************************************************
*  Terminal Display
*  *********************************************************************
*  *********************************************************************

*  If the PAGLEN is not 1, ie not a single line output
         IF ( PAGLEN .GT. 1 ) THEN

*  *********************************************************************
*  Display page headings
*  *********************************************************************
            IF ( .NOT. SURVEY ) THEN
               CALL MSG_OUT( ' ', ' Source    Coord             Pos'//
     :         'ition', STATUS )
               CALL MSG_OUT( ' ', '  Name      Sys      1st Coord'//
     :         '       2nd Coord', STATUS )
            ELSE
               CALL MSG_OUT( ' ', ' Source    Coord             Pos'//
     :         'ition               Region size     Wavebands',
     :         STATUS )
               CALL MSG_OUT( ' ', '  Name      Sys      1st Coord'//
     :         '       2nd Coord    Inscan  Xscan    12 - - 100',
     :         STATUS )
            END IF

*  Calculate the number of the first source to be displayed
            SOTOP = SOBOT - PAGLEN + 1
            IF ( SOTOP .LT. 1 ) THEN
               SOTOP = 1
            END IF

*  Initialise count of number of lines displayed
            NOLINE = 0

         ELSE

*  If a single line is to be output set SOTOP equal to SOBOT
            SOTOP = SOBOT
         END IF

*  If there are sources to be displayed
         IF ( SOBOT .GE. 1 ) THEN

*  *********************************************************************
*  For each source to be displayed
*  *********************************************************************
            DO 100 II = SOTOP, SOBOT

*  Check whether the source is to_be_deleted, and skip the output lines
*  if it is
               IF ( .NOT. SOMADE(II) ) THEN

*  Call subroutine to generate a string containing the source name and
*  coordinate system
                  CALL FIND10( II, STNAME, STATUS )

*  Set up the variables to be included in the output message
                  CALL MSG_FMTC( 'C1', 'A16', STNAME)
                  CALL MSG_FMTC( 'C2', 'A12', SOCO1(II) )
                  CALL MSG_FMTC( 'C3', 'A12', SOCO2(II) )


*  If region size and waveband are required set up necessary variables
                  IF ( SURVEY ) THEN

*  Change the radian values for cross scan and in scan to arc minutes
                     DECROS = SOCRSZ( II ) / AMTOR
                     DEINSC = SOINSZ( II ) / AMTOR

                     CALL MSG_FMTR( 'R4' , 'F5.1', DEINSC)
                     CALL MSG_FMTR( 'R5' , 'F5.1', DECROS)
                     CALL MSG_FMTL( 'L6' , 'L1'  , SOWAB1(II) )
                     CALL MSG_FMTL( 'L7' , 'L1'  , SOWAB2(II) )
                     CALL MSG_FMTL( 'L8' , 'L1'  , SOWAB3(II) )
                     CALL MSG_FMTL( 'L9' , 'L1'  , SOWAB4(II) )
                  END IF

*  *********************************************************************
*  Display line containing source data
*  *********************************************************************
                  IF ( .NOT. SURVEY ) THEN
                     CALL MSG_OUT( ' ', '^C1   ^C2    ^C3',STATUS )
                  ELSE
                     CALL MSG_OUT( ' ',
     :               '^C1   ^C2    ^C3    ^R4  ^R5     ^L6 ^L7 ^L8 ^L9',
     :               STATUS )
                  END IF

*  Increment the number of lines printed
                  NOLINE = NOLINE + 1
               END IF
 100        CONTINUE
         END IF

*  If the PAGLEN is not 1, ie not a single line output
         IF ( PAGLEN .GT. 1 ) THEN

*  Check whether the page is filled
            IF ( NOLINE .LT. MAXLEN) THEN
               DO 200  III = NOLINE, MAXLEN
                  CALL MSG_OUT( ' ', ' ', STATUS )
 200           CONTINUE
            END IF

         END IF

*  *********************************************************************
*  *********************************************************************
*  List to file
*  *********************************************************************
*  *********************************************************************

      ELSE

*  If there are sources to be displayed
         IF ( SOBOT .GE. 1 ) THEN

*  *********************************************************************
*  Display page headings
*  *********************************************************************
            IF ( .NOT. SURVEY ) THEN
               BUFFER( 1 ) = ' Source     Coord             Position'
               CALL FIO_WRITE( FD, BUFFER( 1 ), STATUS )
               BUFFER( 2 ) =
     :         '  Name       Sys      1st Coord       2nd Coord'
               CALL FIO_WRITE( FD, BUFFER( 2 ), STATUS )
            ELSE
               BUFFER( 1 ) = ' Source     Coord             Position'//
     :         '               Region size     Wavebands'
               CALL FIO_WRITE( FD, BUFFER( 1 ), STATUS )
               BUFFER( 2 ) = '  Name       Sys      1st Coord'//
     :         '       2nd Coord    Inscan  Xscan    12 - - 100'
               CALL FIO_WRITE( FD, BUFFER( 2 ), STATUS )
            END IF

*  Put a space character in the carrage return character variable CARRET
            CARRET = ' '

*  *********************************************************************
*  For each source to be displayed
*  *********************************************************************

            DO 300 II = 1, SOBOT

*  Check whether the source is to_be_deleted, and skip the output lines
*  if it is
               IF ( .NOT. SOMADE(II) ) THEN

*  Call subroutine to generate a string containing the source name and
*  coordinate system
                  CALL FIND10( II, STNAME, STATUS )

*  Slect output format depending on whether region size and waveband
*  are required
                  IF ( .NOT. SURVEY ) THEN

*  Set up the buffer for the output message for output without waveband
*  and size
                     WRITE ( BUFFER( 3 ), 9999, IOSTAT = IOS )
     :               CARRET, STNAME, SOCO1(II) , SOCO2(II)

 9999                FORMAT ( A1, A16, 3X, A12, 4X, A12 )

*  Write out buffer to file
                     CALL FIO_WRITE( FD, BUFFER( 3 ), STATUS )

                  ELSE
*  If size and wavebands are required
*  Change the radian values for cross scan and in scan to arc minute
                     DECROS = SOCRSZ( II ) / AMTOR
                     DEINSC = SOINSZ( II ) / AMTOR

*  Set up the buffer for the output message for output with waveband
*  and size
                     WRITE ( BUFFER( 3 ), 9998, IOSTAT = IOS )
     :               CARRET, STNAME, SOCO1(II) , SOCO2(II),
     :               DEINSC, DECROS,
     :               SOWAB1(II), SOWAB2(II), SOWAB3(II), SOWAB4(II)

 9998                FORMAT ( A1, A16, 3X, A12, 4X, A12, 4X,
     :               F5.1, 2X, F5.1, 5X, L1, 1X, L1, 1X, L1, 1X, L1 )

*  Write out buffer to file
                     CALL FIO_WRITE( FD, BUFFER( 3 ), STATUS )

                  END IF

               END IF
 300        CONTINUE
         END IF

*  If there is an error in internal reads for file output
         IF ( IOS .NE. 0 ) THEN
            CALL ERR_REP( ' ', 'The source details have not been'//
     :      ' saved to a listable file because of output error '//
     :      ' - program continuing', STATUS )
         END IF

*  *********************************************************************
*  End if for type of display
*  *********************************************************************

      END IF

      END

