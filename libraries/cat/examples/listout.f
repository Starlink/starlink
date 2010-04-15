*+  LISTOUT - List values of named fields for objects in catalogue
      SUBROUTINE LISTOUT (STATUS)
*    Description :
*     Lists the values of named fields for all the objects in a
*     StarBase catalogue. The output is sent either to a user-defined
*     formatted file or to the interim environment, or both.
*
*     Note that this application has been hacked together from the SCAR
*     application of the same name.  It has old-style subroutine
*     prologues.
*    Invocation :
*     CALL LISTOUT (STATUS)
*    Parameters :
*     CNAME  =  CHARACTER (ENTRY)
*           Catalogue name.
*     LISTMODE  =  CHARACTER (ENTRY)
*           Output to be sent to the terminal screen, a file or both?
*     SWIDTH  =  INTEGER (ENTRY)
*           Required width of screen listing.  The permitted range is
*           32 to 75, inclusive.
*     HEADER  =  LOGICAL (ENTRY)
*           Should the output have a header giving column names etc?
*     WIDTH  = INTEGER (ENTRY)
*           Give required width of printed report.  The permitted range
*           is 32 to 160, inclusive.
*     NUMBER  =  LOGICAL (ENTRY)
*           Include a running record count in the output?
*     FILOUT  =  CHARACTER (ENTRY)
*           The name of the text file to which output is directed.
*     SPACE  =  INTEGER (ENTRY)
*           The number of blank spaces inserted between columns.
*     CHOOSE  =  CHARACTER (ENTRY)
*           Choose a column to be included in the listing.
*    Method :
*     If (status ok) then
*       attempt to get catalogue identifier
*       if (status ok) then
*         determine whether screen display, file or both
*         if (screen only) then
*           set flag; a header is to be produced.
*           obtain maximum screen width (SWIDTH)
*         else if (file only) then
*           obtain the header flag
*           obtain maximum width (FWIDTH)
*         else if (both) then
*           obtain the header flag
*           obtain maximum screen width (SWIDTH)
*         end if
*         obtain images to be numbered flag
*         if (.not. screen only) then
*           Obtain the name of the required output file.
*           attempt to open file
*           if (status not ok) then
*             report error
*             set general error status
*           end if
*         end if
*       end if
*       if (status ok) then
*         obtain spacing between columns
*         if (number) then
*           reduce width available
*         end if
*         obtain required column names and identifiers
*         if (status ok and the number of required columns .gt. 0) then
*           produce the listing
*         else
*           if (status ok and number of fields is 0) then
*             report 0 columns given.
*           end if
*         end if
*       end if
*       if an output file is being produced then
*         attempt to close the output file.
*       end if
*       release the catalogue identifier.
*     end if
*    Deficiencies
*    Bugs :
*     None known.
*    Authors :
*     B D Kelly.           (ROE::BDK)
*     A C Davenhall.       (ROE::ACD, LEI::ACD)
*     S M Beard.           (ROE::SMB)
*     S K Leggett          (ROE::SKL)
*    History :
*     17/2/83:    Original version.                             (ROE::BDK)
*     15/2/84:    Modified.                                     (ROE::ACD)
*     19/6/84:       "    .                                     (ROE::ACD)
*     7/7/84:     Modified to full SSE style and so that        (ROE::ACD)
*                 does not crash if given 0 attributes.
*     19/9/84:    HEADER and NUMBER options added               (ROE::SMB)
*     18/10/84:   Redirection to VMS file simplified            (ROE::SMB)
*     19/10/84:   Re-written in a more modular style.           (ROE::ACD)
*                 Options rationalised, and an option
*                 for re-formatting angular coord.
*                 attributes added.
*     25/10/84:   Give an option for the user to control        (ROE::ACD)
*                 the width of each column.
*     25/3/87:    Converted from HAGGIS to SCAR                 (ROE::SKL)
*     25/4/90:    Lists all fields and runs in batch                   ???
*     23/9/93:    Converted to StarBase.                        (LEI::ACD)
*     14/10/93:   First working StarBase version.               (LEI::ACD)
*     21/2/94:    Removed unused variables.                     (LEI::ACD)
*     19/4/95:    Changed to parametric constants to correspond (LEI::ACD)
*                 to changes to INCLUDE file CAT_PAR.
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'CAT_PAR'
*    Status :
      INTEGER
     :  STATUS        ! Running status.
*    External references :
*    Global variables :
*    Local Constants :
      INTEGER
     :  FLUNIT,       ! Unit number for output file
     :  NUMLEN        ! Length of number string for output
      PARAMETER (FLUNIT = 10)
      PARAMETER (NUMLEN =  8)
*    Local variables :
      CHARACTER
     :  CNAME*(CAT__SZCNF),            ! Catalogue name
     :  FILOUT*80,                     ! Name of output file
     :  NAMEA(CAT__MXCOL)*(CAT__SZCMP),! Names of the required columns
     :  MODE*10,                       ! Screen, file or both?
     :  DESCR(3)*50,                   ! Description of mode responses
     :  RESPS(3)*10,                   ! Possible responses
     :  UNITS(CAT__MXCOL)*(CAT__SZUNI) ! Units of the required fields
      INTEGER
     :  CI,                ! Catalogue identifier
     :  WIDTH,             ! Maximum width for output
     :  OSTAT,             ! Status on file open
     :  CSTAT,             ! Status on file close
     :  ACTWTH,            ! Actual available width for output
     :  SPACE,             ! Column spacing
     :  NUMFLD,            ! Number of fields
     :  FIA(CAT__MXCOL),   ! Identifiers for the required columns
     :  CWIDTH(CAT__MXCOL) !Column width for required fields
      LOGICAL
     :  HEADER,            ! Is a header wanted for formatted file?
     :  HEAD,              ! Header flag for column width
     :  NUMBER             ! Is numbering wanted?
*    Internal References :
*    Local data :
      DATA DESCR/'Screen display of catalogue contents',
     :       'Formatted file of catalogue contents',
     :       'Screen display and file of catalogue contents'/,
     :     RESPS/'S#CREEN', 'F#ILE', 'B#OTH'/
      SAVE DESCR, RESPS
*-

      IF (STATUS .EQ. SAI__OK) THEN


*       Attempt to get catalogue descriptor
          CALL PAR_GET0C ('CNAME', CNAME, STATUS)
          CALL PAR_CANCL ('CNAME', STATUS)

          CALL CAT_TOPEN (CNAME, 'OLD', 'READ', CI, STATUS)

          IF (STATUS .EQ. SAI__OK) THEN

*           Ask user whether they want a screen display, file or both
              CALL CAR_CHOIC ('LISTMODE', .TRUE.,
     :          'Do you want a screen display, '/
     :          /'formatted file, or both?', 3, DESCR, RESPS, MODE,
     :          STATUS)


              IF (MODE .EQ. 'SCREEN') THEN
                  HEADER = .TRUE.
                  CALL PAR_GET0I ('SWIDTH', WIDTH, STATUS)
                  CALL PAR_CANCL ('SWIDTH', STATUS)

              ELSE IF (MODE .EQ. 'FILE') THEN
                  CALL PAR_GET0L ('HEADER', HEADER, STATUS)
                  CALL PAR_CANCL ('HEADER', STATUS)

                  CALL PAR_GET0I ('WIDTH', WIDTH, STATUS)
                  CALL PAR_CANCL ('WIDTH', STATUS)

              ELSE IF (MODE .EQ. 'BOTH') THEN
                  CALL PAR_GET0L ('HEADER', HEADER, STATUS)
                  CALL PAR_CANCL ('HEADER', STATUS)

                  CALL PAR_GET0I ('SWIDTH', WIDTH, STATUS)
                  CALL PAR_CANCL ('SWIDTH', STATUS)

              END IF

              CALL PAR_GET0L ('NUMBER', NUMBER, STATUS)
              CALL PAR_CANCL ('NUMBER', STATUS)


              IF (MODE .NE. 'SCREEN') THEN

*               Obtain the name of the required output file.
                  CALL PAR_GET0C ('FILOUT', FILOUT, STATUS)
                  CALL PAR_CANCL ('FILOUT', STATUS)

*               attempt to open file
                  OPEN (FLUNIT, FILE=FILOUT, STATUS='NEW', IOSTAT=OSTAT)
                  CALL FIO_SERR (OSTAT, STATUS)
                  IF (STATUS .NE. SAI__OK) THEN
                      CALL ERR_REP (' ', 'ERROR opening output file',
     :                  STATUS)
                  END IF

              END IF

          END IF

          IF (STATUS .EQ. SAI__OK) THEN

*           Obtain column spacing
              CALL PAR_GET0I ('SPACE', SPACE, STATUS)
              CALL PAR_CANCL ('SPACE', STATUS)

*           If output records are to be numbered then reduce available
*           width
              IF (NUMBER) THEN
                ACTWTH = WIDTH - (NUMLEN + 2 + SPACE)
              ELSE
                ACTWTH = WIDTH
              END IF

*           If output to both screen and file, or header required, column
*           width must allow for header
              IF (HEADER .OR. MODE .EQ. 'BOTH') THEN
                  HEAD = .TRUE.
              END IF

*           Get the list of columns which are to be listed.
              CALL CAR_GTFLD (CI, CAT__MXCOL, NUMFLD, NAMEA, FIA, UNITS,
     :          CWIDTH, STATUS)

              IF (STATUS .EQ. SAI__OK .AND. NUMFLD .GT. 0) THEN

                  CALL CAR_GTLST (CI, HEADER, NUMBER, NUMLEN, WIDTH,
     :              NUMFLD, NAMEA, FIA, UNITS, MODE, FLUNIT, CWIDTH,
     :              SPACE, STATUS)

              ELSE
                  IF (NUMFLD .LE. 0) THEN
                      CALL MSG_OUT (' ', 'No columns have been given',
     :                  STATUS)
                  END IF
              END IF
          END IF


          IF (MODE .NE. 'SCREEN') THEN
              CLOSE (FLUNIT, STATUS='KEEP', IOSTAT=CSTAT)
              CALL FIO_SERR (CSTAT, STATUS)

              IF (STATUS .NE. SAI__OK) THEN
                  CALL ERR_REP (' ', 'ERROR closing output file',
     :              STATUS)
              END IF

          END IF


*       Release the catalogue identifier.
          CALL CAT_TRLSE (CI, STATUS)

      END IF

      END
