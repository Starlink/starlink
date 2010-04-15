*+  RED4_READ_EMLT2 - Reads and extracts information from an EMLT.LIS file.
      SUBROUTINE RED4_READ_EMLT2( XMIN_D, XMAX_D, BCEN_D, XCEN_D,
     :  BFWHM_D, XFWHM_D, STRENGTH_D, PEAK_D, MEANBFWHM_D, MEANXFWHM_D, STATUS )
*    Description :
*     This routine opens the latest version of the file EMLT.LIS in the
*     current directory and extracts information from that file
*     describing the centre and width of the brightest line within
*     the desired X range.
*
*     This routine is called by the RED4 D-task, but it has been
*     designed to stand alone in any ADAM task. The routine uses
*     ERR_ and MSG_ calls, so message reporting should be turned on.
*
*     EMLT.LIS is produced by the FIGARO EMLT function, which locates
*     emission lines. Unfortunately, this function writes all its
*     results to a file and does not return any parameters. This routine
*     is needed to correct that omission.
*
*     Here is a typical EMLT.LIS file :-
*
*Centre of      Centroid          Integrated
*  moment                          Strength
*
*   169.6          169.9         0.6425E+05
*   182.7          182.7         0.1464E+07
*   194.5          194.5          8638.
*   201.4          201.3          3787.
*
*    Bin                    Width      Integrated    Peak
*  Number               Bins            Strength    Height
*
*  170.33       42.33   3.75      0.94 1.606E+04 1.608E+04
*  182.78       45.44   4.32      1.08 3.661E+05 3.187E+05
*  199.17       49.54   2.32      0.58 9.468E+02 1.531E+03
*
*Mean fullwidth at half maximum =  4.280     bins
*                               =  1.070
*
*    Invocation :
*      CALL RED4_READ_EMLT2( XMIN_D, XMAX_D, BCEN_D, XCEN_D,
*     :  BFWHM_D, XFWHM_D, STRENGTH_D, PEAK_D, MEANBFWHM_D, MEANXFWHM_D, STATUS )
*    Parameters :
*     XMIN_D        = REAL( READ )
*           The minimum acceptable X position for the line
*     XMAX_D        = REAL( READ )
*           The maximum acceptable X position for the line
*     BCEN_D        = REAL( WRITE )
*           The centre of the line in bin units (i.e. data array
*           element units).
*     XCEN_D        = REAL( WRITE )
*           The centre of the line in X position units (i.e. the
*           units of the X axis array).
*     BFWHM_D       = REAL( WRITE )
*           The full width at half maximum for the line in bin units.
*     XFWHM_D       = REAL( WRITE )
*           The full width at half maximum for the line in X units.
*     STRENGTH_D    = REAL( WRITE )
*           The integrated strength of the line (in Y axis units I
*           think).
*     PEAK_D        = REAL( WRITE )
*           The peak height of the line (in Y axis units I think).
*     MEANBFWHM_D   = REAL( WRITE )
*           The mean FWHM of ALL the lines in the file in bin units.
*     MEANXFWHM_D   = REAL( WRITE )
*           The mean FWHM of ALL the lines in the file in X units.
*     STATUS      = INTEGER( UPDATE )
*           Global status. The routine will only execute if
*           this is SAI__OK on entry. There are three possible
*           status returns:
*           SAI__OK    - The routine has completed successfully
*           SAI__ERROR  - A fatal error has occurred
*           SAI__WARN   - An error has occurred while obtaining
*                         the mean FWHM values, but the other data
*                         values have been obtained successfully.
*    Method :
*     <description of how the subroutine works>
*    Deficiencies :
*     Note that this routine is heavily dependent upon EMLT.LIS
*     having a particular format. If the format changes, this
*     routine will fail.
*    Authors :
*     S.M.Beard   (REVAD::SMB)
*     P.N.Daly    (JACH::PND)
*    History :
*     11-May-1990: Original version.                               (SMB)
*      2-Jul-1990: Bug fix. FOR__ENDDUREA replace by FOR__OK.      (SMB)
*     26-Jul-1990: EMLT sometimes writes a "--" into the fields that
*                  are supposed to be the line centre and FWHM in
*                  X units, and this was causing problems. Code
*                  modified so if this happens, the line centre an
*                  FWHM in X units are made the same as those in
*                  bin units.                                      (SMB)
*     26-Jul-1990: Further bug fix: If the X units fields are "--"
*                  then a second FWHM record will not exist at the
*                  end. This knowledge included.                   (SMB)
*     24-Aug-1992: Correct ERR_REP problem.                        (PND)
*     18-Feb-1993: Remove LIB$ rtl calls                           (PND)
*     19-Jan-1995: Ported to Unix                                  (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'FIO_ERR'
*    Import :
      REAL XMIN_D, XMAX_D
*    Export :
      REAL
     :  BCEN_D,
     :  XCEN_D,
     :  BFWHM_D,
     :  XFWHM_D,
     :  STRENGTH_D,
     :  PEAK_D,
     :  MEANBFWHM_D,
     :  MEANXFWHM_D
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'
*    External references :
      INTEGER CHR_LEN
*    Local Constants :
      INTEGER
     :  MAXLINES                     ! The maximum number of lines allowed
      PARAMETER ( MAXLINES = 32 )
      INTEGER
     :  MAXWRD                       ! Maximum number of words
      PARAMETER ( MAXWRD = 6 )
*    Local variables :
      CHARACTER*80
     :  FILE,                        ! Filename
     :  STRING,                      ! Record read from the file
     :  SUBSTR,                      ! Substring extracted from string
     :  WORDS( MAXWRD )              ! Words extracted from record
      INTEGER
     :  START( MAXWRD ),             ! First character position of words
     :  STOP( MAXWRD ),              ! Last character position of words
     :  NUMWRD,                      ! Number of words
     :  EQPOS,                       ! Position of '=' sign
     :  NLINES,                      ! Number of lines
     :  I, CLEN,                     ! DO loop counter
     :  STRONGEST,                   ! Pointer to the strongest line
     :  LUNIT                        ! Logical unit number
      REAL
     :  MAX_STRENGTH_D,                ! Maximum strength
     :  BCEN_D_ARRAY( MAXLINES ),      ! Array to hold centres in bins
     :  XCEN_D_ARRAY( MAXLINES ),      ! Array to hold centres in X units
     :  BFWHM_D_ARRAY( MAXLINES ),     ! Array to hold FWHMs in bins
     :  XFWHM_D_ARRAY( MAXLINES ),     ! Array to hold FWHMs in X
     :  STRENGTH_D_ARRAY( MAXLINES ),  ! Array to hold strengths
     :  PEAK_D_ARRAY( MAXLINES )       ! Array to hold peaks
      LOGICAL
     :  X_UNITS_PRESENT              ! Flag which is set when the EMLT file
*                                    !   contains info for X units.
*                                    !   If this flag is .FALSE. a second FWHM
*                                    !   record will not exist at the end.
*-

*    Abort if error on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Attempt to open the latest version of $CGS4_ENG/emlt.lis
      CALL CHR_FILL( ' ', FILE )
      FILE = CGS4_ENG(1:CHR_LEN(CGS4_ENG)) // 'emlt.lis'
      CALL CHR_RMBLK( FILE )
      CALL FIO_OPEN( FILE(1:CHR_LEN(FILE)), 'READ', 'LIST', 132, LUNIT, STATUS )

*    Read the file until a record containing the strings 'Number',
*    'Bins', 'Strength' and 'Height' is found, or until an error
*    occurs. (This is a very strict condition. Any major change
*    to the file format will cause this to fail).
      CALL FIO_READ( LUNIT, STRING, CLEN, STATUS )
      DO WHILE ( STATUS.EQ.SAI__OK  .AND.
     :           ( INDEX( STRING, 'Number').EQ.0 .OR. INDEX( STRING, 'Bins').EQ.0 .OR.
     :             INDEX( STRING, 'Strength').EQ.0 .OR. INDEX( STRING, 'Height').EQ.0 ) )
         CALL FIO_READ( LUNIT, STRING, CLEN, STATUS )
      ENDDO
      IF ( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )

*    Initialise the number of lines.
      NLINES = 0

*    Skip any blank records in the file
      CALL FIO_READ( LUNIT, STRING, CLEN, STATUS )
      DO WHILE ( STATUS.EQ.SAI__OK .AND. STRING.EQ.' ' )
         CALL FIO_READ( LUNIT, STRING, CLEN , STATUS )
      ENDDO
      IF ( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )

*    If the file is in the correct format, we should now
*    have reached the region of interest. Each record will
*    have six numbers. There will be one record for each
*    emission line described, terminated by a blank record.
*    Initialise the X units present flag. (Assume X units information
*    is present unless a "--" is found in the file where the X units should be).
      X_UNITS_PRESENT = .FALSE.

*    Keep looping until the decoding of the information is
*    complete or until an error occurs.
      DO WHILE ( STATUS.EQ.SAI__OK .AND. STRING.NE.' ' )

*       Decode the current record into words.
         CALL CHR_DCWRD( STRING, MAXWRD, NUMWRD, START, STOP, WORDS, STATUS )

*       Check that the correct number of words have been found.
         IF ( NUMWRD .EQ. MAXWRD ) THEN

*          Increment the number of lines, and check the maximum has not been reached.
            NLINES = NLINES + 1

            IF ( NLINES .LE. MAXLINES ) THEN

*             Attempt to convert each one of the words into a floating
*             point number, and load up the appropriate array elements.
*             N.B. The XCEN_D and XFWHM_D values can sometimes contain "--"
*             to indicate there is no X calibration. In this case set
*             these values to the same as the ones in bin units, and
*             also unset the X_UNITS_PRESENT flag to show there will
*             not be a second FWHM record at the end.
               CALL CHR_CTOR( WORDS(1), BCEN_D_ARRAY(NLINES), STATUS )
               IF ( INDEX( WORDS(2), '--' ) .EQ. 0 ) THEN
                  CALL CHR_CTOR( WORDS(2), XCEN_D_ARRAY(NLINES), STATUS )
               ELSE
                  XCEN_D_ARRAY(NLINES) = BCEN_D_ARRAY(NLINES)
                  X_UNITS_PRESENT = .FALSE.
               ENDIF

               CALL CHR_CTOR( WORDS(3), BFWHM_D_ARRAY(NLINES),STATUS )
               IF ( INDEX( WORDS(2), '--' ) .EQ. 0 ) THEN
                  CALL CHR_CTOR( WORDS(4), XFWHM_D_ARRAY(NLINES), STATUS )
               ELSE
                  X_UNITS_PRESENT = .FALSE.
                  XFWHM_D_ARRAY(NLINES) = BFWHM_D_ARRAY(NLINES)
               ENDIF

               CALL CHR_CTOR( WORDS(5), STRENGTH_D_ARRAY(NLINES), STATUS )
               CALL CHR_CTOR( WORDS(6), PEAK_D_ARRAY(NLINES), STATUS )

*             If the line is outside the desired X range, then ignore it.
               IF ( XCEN_D_ARRAY(NLINES).LT.XMIN_D  .OR. XCEN_D_ARRAY(NLINES).GT.XMAX_D ) NLINES = NLINES - 1
            ELSE
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_READ_EMLT2: Too many lines found '/
     :              /'in EMLT.LIS - reduce X range', STATUS )
            ENDIF
         ELSE
*          An unexpected number of words have been found.
*          There must be a mistake in the file format.
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_READ_EMLT2: '/
     :        /'Invalid $CGS4_ENG/emlt.lis file format', STATUS )
            CALL ERR_REP( ' ', 'RED4_READ_EMLT2: '/
     :        /'Incorrect number of words in line fit records', STATUS )
         ENDIF

*       Read the next line. (The DO loop will only continue if the line read is not blank).
         CALL FIO_READ( LUNIT, STRING, CLEN, STATUS )
      ENDDO

*    What happens next depends on the number of suitable lines found.
      IF ( NLINES .LE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FILE', FILE )
         CALL ERR_REP( ' ', 'RED4_READ_EMLT2: No suitable lines found in ^FILE', STATUS )

      ELSE IF ( NLINES .EQ. 1 ) THEN
         BCEN_D     = BCEN_D_ARRAY( NLINES )
         XCEN_D     = XCEN_D_ARRAY( NLINES )
         BFWHM_D    = BFWHM_D_ARRAY( NLINES )
         XFWHM_D    = XFWHM_D_ARRAY( NLINES )
         STRENGTH_D = STRENGTH_D_ARRAY( NLINES )
         PEAK_D     = PEAK_D_ARRAY( NLINES )
      ELSE

*       More than one line has been found. Determine the
*       strongest line, and return the parameters for that.
         MAX_STRENGTH_D = STRENGTH_D_ARRAY(1)
         STRONGEST = 1
         DO I = 2, NLINES
            IF ( STRENGTH_D_ARRAY(I) .GT. MAX_STRENGTH_D ) THEN
               MAX_STRENGTH_D = STRENGTH_D_ARRAY(I)
               STRONGEST = I
            ENDIF
         ENDDO

         BCEN_D     = BCEN_D_ARRAY( STRONGEST )
         XCEN_D     = XCEN_D_ARRAY( STRONGEST )
         BFWHM_D    = BFWHM_D_ARRAY( STRONGEST )
         XFWHM_D    = XFWHM_D_ARRAY( STRONGEST )
         STRENGTH_D = STRENGTH_D_ARRAY( STRONGEST )
         PEAK_D     = PEAK_D_ARRAY( STRONGEST )
      ENDIF

*    Read the file until a line containing a '=' is found.
      CALL FIO_READ( LUNIT, STRING, CLEN, STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK .AND. INDEX(STRING,'=').EQ.0 )
         CALL FIO_READ( LUNIT, STRING, CLEN, STATUS )
      ENDDO

*    The first word immediately after the '=' sign will
*    contain the mean FWHM in bin units.
      EQPOS  = INDEX( STRING, '=' )
      SUBSTR = STRING( EQPOS+1: )
      CALL CHR_DCWRD( SUBSTR, MAXWRD, NUMWRD, START, STOP, WORDS, STATUS )
      CALL CHR_CTOR( WORDS(1), MEANBFWHM_D, STATUS )

*    Check if a second FWHM record is expected.
      IF ( X_UNITS_PRESENT ) THEN

*       Read the next line. It should contain the mean FWHM in
*       X units immediately after the '=' sign.
         CALL FIO_READ( LUNIT, STRING, CLEN, STATUS )
         EQPOS  = INDEX( STRING, '=' )
         SUBSTR = STRING( EQPOS+1: )
         CALL CHR_DCWRD( SUBSTR, MAXWRD, NUMWRD, START, STOP, WORDS, STATUS )
         CALL CHR_CTOR( WORDS(1), MEANXFWHM_D, STATUS )
      ELSE

*       If there is not a second FWHM record expected,
*       set the mean FWHM in X units the same as that
*       found in bin units.
         MEANXFWHM_D = MEANBFWHM_D
      ENDIF
      CALL FIO_CLOSE( LUNIT, STATUS )
      END
