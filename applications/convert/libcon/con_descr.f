      SUBROUTINE CON_DESCR( BDF, NDF, NDIM, WRDSCR, FLOC, CRVAL, CDELT,
     :                      CRPIX, CRTYPE, AXTHER, TITLE, LABEL, UNITS,
     :                      STATUS )
*+
*  Name:
*     CON_DESCR

*  Purpose:
*     Copies BDF descriptors to NDF FITS extension.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_DESCR( BDF, NDF, NDIM, WRDSCR, FLOC, CRVAL, CDELT, CRPIX,
*                     CRTYPE, AXTHER, TITLE, LABEL, UNITS, STATUS)

*  Description:
*     The BDF descriptors are copied to the NDF FITS extension.
*     The values of certain descriptors are returned to the
*     calling program. These are the TITLE, LABEL, and UNITS, which are
*     written to the output NDF; and the CRVALn, CDELTn, CRPIXn, and
*     CRTYPEn values which can be used to generate axis structures in
*     the output NDF. See SUN/55 for details.

*  Arguments:
*     BDF = CHARACTER*(*) (Given)
*        The input BDF parameter name.
*     NDF = INTEGER (Given)
*        The NDF identifier.
*     NDIM = INTEGER (Given)
*        Dimensions of the NDF main data array.
*     WRDSCR = LOGICAL (Given)
*        If true, the descriptors are reported to the user.
*     FLOC = CHARACTER*(DAT__SZLOC) FLOC  (Returned)
*        Locator to the MORE.FITS structure.
*     CRVAL( DAT__MXDIM ) = REAL (Returned)
*        Values of the CRVALn descriptors.  These default to 0.5
*     CDELT( DAT__MXDIM ) = REAL (Returned)
*        Values of the CDELTn descriptors.  These default to 1.0
*     CRPIX( DAT__MXDIM ) = REAL (Returned)
*        Values of the CRPIXn descriptors.  These default to 1.0.
*     CRTYPE( DAT__MXDIM ) = CHARACTER * ( * )(Returned)
*        Values of the CRTYPEn descriptors.  These default to blank
*        strings.
*     AXTHER( DAT__MXDIM ) = LOGICAL (Returned)
*        Indicates whether or not CRVALn and CDELTn are present
*        to form an axis.  CROTAn must also be absent or equal to zero.
*     TITLE = CHARACTER*72 (Returned)
*        Value of TITLE descriptor.
*     LABEL = CHARACTER*72 (Returned)
*        Value of LABEL descriptor.
*     UNITS = CHARACTER*72 (Returned)
*        Value of BUNITS descriptor.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     Truncated logical and numeric descriptors have a comment string
*     in the FITS card image indicating the fact.  Truncated characted
*     strings are indicated by an ellipsis following the value.

*  Copyright:
*     Copyright (C) 1990-1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     JM: Jo Murray (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 Feb 8 (JM):
*        Adapted from INTERIM routine CPYDSC.
*     1990 March 27 (MJC):
*        Fixed deficiency when handling arrays of descriptors.
*     1991 Feb 11 (JM)
*        Values of certain descriptors returned to calling program.
*        TITLE and LABEL descriptor values returned to provide NDF
*        TITLE and LABEL.  CRVALn and CDELTn values returned to generate
*        AXIS(n) structures in output NDF.
*     1992 January 30 (MJC):
*        Passed further descriptor values to the calling routine:
*        UNITS from the BUNITS descriptor, CRPIX and CRTYPE.  Tidied
*        the prologue.  Removed the PAR_GET0L to the calling routine,
*        so that the parameter name is visible there and is outside
*        the main descriptor loop; therefore passed WRDSCR as an
*        argument.  Truncation of character descriptors increased from
*        20 to 65 (since there are no comments). Inserted mandatory
*        apostrophes around the FITS character values, and made the
*        minimum length 8 characters.  Added Notes item.  Initialised
*        all axis-related variables.
*     1992 July 20 (MJC):
*        Obtained the full set of descriptor names and values by a new
*        subroutine, rather than by Interim routine RDDSCN, which cannot
*        handle fragmented arrays of descriptors such as appear in
*        La Palma FITS headers.  Handle verbatim FITS card images in
*        the descriptors.
*     1992 November 17 (MJC):
*        Rotated axes are flagged as not being present, as these cannot
*        be handled by the NDF in a standard way.
*     {enter_further_changes_here}

*  Bugs:
*     -  May give spurious error messages if running under ICL. This
*     happens if a non-existent BDF is given as the input file.
*     A subsequent invocation of the program may result in a repeat
*     of the error message although the conversion is carried out
*     correctly.
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE                ! No implicit typing

*  Global Constants:
      INCLUDE   'SAE_PAR'           ! Standard SAE constants
      INCLUDE   'PRM_PAR'           ! PRIMDAT public constants
      INCLUDE   'INTERIM(ERRPAR)'   ! Interim error constants

*  Arguments Given:
      CHARACTER BDF*(*)             ! Reference to BDF dataset
      INTEGER   NDF                 ! NDF identifier
      INTEGER   NDIM                ! Dimensions of data array
      LOGICAL   WRDSCR              ! True if descriptors to be reported

*  Arguments Reurned:
      CHARACTER TITLE*72            ! Title
      CHARACTER LABEL*72            ! Label
      CHARACTER UNITS*72            ! Units
      REAL      CRVAL(DAT__MXDIM)   ! Accommodates CRVALn values
      REAL      CDELT(DAT__MXDIM)   ! Acommodates CDELTn values
      REAL      CRPIX(DAT__MXDIM)   ! Accommodates CRPIXn values
      CHARACTER*(*) CRTYPE(DAT__MXDIM)  ! Accommodates CRTYPEn values
      LOGICAL   AXTHER(DAT__MXDIM)  ! Indicates if axis info. found
      CHARACTER FLOC*(DAT__SZLOC)   ! Locator to MORE.FITS structure

*  Status:
      INTEGER   STATUS              ! Global status

*  External References:
      INTEGER   CHR_LEN             ! Get effective length of string

*  Local constants:
      INTEGER   SZDESC              ! Size of descriptors
      PARAMETER( SZDESC = 72 )
      INTEGER   MAXELS              ! Maximum size descriptor array
      PARAMETER( MAXELS = 4000 )
      CHARACTER * ( 1 ) APS         ! Apostrophe
      PARAMETER ( APS = '''' )

*  Local Variables:
      CHARACTER C * ( 1 )           ! Accommodates single character
      LOGICAL   CDTHER(DAT__MXDIM)  ! True if CDELTn values found
      INTEGER   COMCOL              ! Column containing a FITS comment
                                    ! delimiter
      INTEGER   COPCOL              ! Number of characters to copy in
                                    ! a character value before adding
                                    ! padding and trailing quote
      LOGICAL   CRTHER(DAT__MXDIM)  ! True if CRVALn values found
      CHARACTER DESCR * ( 8 )       ! Name of descriptor item
      INTEGER   EXTSPA              ! Number of Extra spaces between
                                    ! the value and the comment
                                    ! delimiter, above the default of 1
      CHARACTER FITSTR * ( 80 )     ! Accommodates FITS string
      CHARACTER FLOCI * ( DAT__SZLOC ) ! Locator to Ith item in
                                    ! MORE.FITS
      CHARACTER FVAL * ( 20 )       ! Accommodates FITS value
      INTEGER   I                   ! Loop variable
      INTEGER   IAX                 ! Loop variable
      INTEGER   ISTAT               ! Local status value
      INTEGER   KOUNT               ! Loop variable
      INTEGER   LENVAL              ! Length of a character value
      LOGICAL   LOOP                ! True means continue to process
                                    ! descriptors
      LOGICAL   LOST                ! True if information lost
      LOGICAL   LVALUE              ! Logical value
      INTEGER   N                   ! Number of FITS descriptors
      INTEGER   NCHAR               ! Number of characters
      INTEGER   NPAD                ! Number of blanks to pad character
                                    ! value
      CHARACTER NAMES( MAXELS ) * ( 8 )! Names of descriptor items
      LOGICAL   NOROT(DAT__MXDIM)   ! True if CROTAn values are absent
                                    ! or equal to zero
      INTEGER   PADCOL              ! Column where padding starts or
                                    ! ends
      INTEGER   QUOCOL              ! Column containing trailing quote
      REAL      RVALUE              ! Real value
      CHARACTER TEST1 * ( 6 )       ! Used to compare with Descriptors
      CHARACTER TEST2 * ( 6 )       ! Used to compare with Descriptors
      CHARACTER TEST3 * ( 6 )       ! Used to compare with Descriptors
      CHARACTER TEST4 * ( 7 )       ! Used to compare with Descriptors
      CHARACTER TEST5 * ( 6 )       ! Used to compare with Descriptors
      CHARACTER VALUE * ( SZDESC )  ! 'Value' of descriptor item
      CHARACTER VALUES( MAXELS ) * ( SZDESC ) ! Values of descriptor
                                    ! items

*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Initialise TITLE, LABEL and UNITS.
      TITLE = ' '
      LABEL = ' '
      UNITS = ' '

*    Initialise flags and values.
      DO I = 1, NDIM
         CRTHER( I ) = .FALSE.
         CDTHER( I ) = .FALSE.
         NOROT( I ) = .TRUE.
         CRTYPE( I ) = ' '
         CRVAL( I ) = 0.5
         CDELT( I ) = 1.0
         CRPIX( I ) = 1.0
      END DO

*   Count the maximum number of descriptors by reading them until
*   an error occurs.  Also store the names and values in arrays.
      CALL CON_RDID( BDF, MAXELS, NAMES, VALUES, N, STATUS )

*   Create a .MORE.FITS structure of the appropriate size.
      CALL NDF_XNEW( NDF, 'FITS', '_CHAR*80', 1, N, FLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*   Initialise the pointers and counters before the main loop.
      I = 0
      DO  KOUNT = 1, N

*   Use a couple of work variables.
         DESCR = NAMES( KOUNT )
         VALUE = VALUES( KOUNT )

*      Report the descriptor name and value.
         IF ( WRDSCR ) THEN
            CALL MSG_SETC( 'DSCNAME', DESCR )
            CALL MSG_SETC( 'VALUE', VALUE )
            CALL MSG_OUT( 'CON_DESCR_DSCVL',
     :        '^DSCNAME : ^VALUE', STATUS )
         END IF

*      Format as a FITS card-image.  There are a number of special
*      cases:  Blank, COMMENT or HISTORY FITS keywords do not need to
*      be formatted with values.  The verbatim value is used in
*      the FITS extension when one of these descriptors is found.
*      These may have blank values, which is tested by the effective
*      length of the value.  An example is a completely blank card
*      image i.e. blank descriptor and value should be just a blank
*      line in the FITS extension.
         LENVAL = CHR_LEN( VALUE )
         IF ( DESCR .EQ. ' ' .OR. DESCR( 1:7 ) .EQ. 'HISTORY' .OR.
     :        DESCR( 1:7 ) .EQ. 'COMMENT' ) THEN
            IF ( LENVAL .EQ. 0 ) THEN
               FITSTR = ' '
               FITSTR = DESCR( 1:8 )
            ELSE
               FITSTR = DESCR( 1:8 )//' '//VALUE( 1:MIN( 71, LENVAL ) )
            END IF

*         Get locator to appropriate element in .MORE.FITS structure.
            I = I + 1
            CALL DAT_CELL( FLOC, 1, I, FLOCI, STATUS )

*         Write the FITS card image to .MORE.FITS
            CALL DAT_PUT0C( FLOCI, FITSTR, STATUS )

*      Format the "normal" descriptors for the FITS extension.
         ELSE

*         Transcribe the descriptor name to the FITS keyword.
*         Initialise the value.
            FITSTR( 1:10 ) = DESCR(1:8)//'= '
            FITSTR( 11:80 ) = ' '

*         Try to deal with a special case where someone has put in the
*         FITS header verbatim into the descriptors, rather than values
*         as is normally done.  There are two distinguishing features:
*         a) a quote in the first column indicating a character value,
*         and b) a backslash after the value to delimit a comment.  The
*         latter is not straightforward because a character string may
*         contain a backslash, and if the value is numeric, the leading
*         spaces may have been lost in the output so a comment follows
*         immediately the value and a space.  Use the space to help
*         reduce the number of incorrectly parsed descriptor values.
*         If the descriptor does not have these but was formatted
*         verbatim, the default coding will handle these cases.
            COMCOL = INDEX( VALUE, ' /' )

*         In theory given a quote the whole line can be copied
*         completely, as there is text is left-justified.  However, we
*         have to ensure that the strings are of length at least eight
*         characters (plus a leading and trailing quote).
            IF ( VALUE( 1:1 ) .EQ. APS ) THEN

*             The string is long enough so just copy the whole value
*             into the FITS card image.
               IF ( LENVAL .GE. 10 ) THEN
                  FITSTR( 11:80 ) = VALUE( 1:70 )

               ELSE

*               Find the number of spaces required to pad out the value
*               to conform with FITS rules.
                  NPAD = 10 - LENVAL

*               See whether or not there is a trailing quote, and hence
*               determine how many characters are to be transferred.
                  QUOCOL = INDEX( VALUE( 2: ), APS )
                  IF ( QUOCOL .EQ. 0 ) THEN
                     COPCOL = LENVAL
                  ELSE
                     COPCOL = LENVAL - 1
                  END IF

*               Copy the value up to, but not including, the trailing
*               quote, if present.
                  PADCOL = 11 + COPCOL
                  FITSTR( 11:PADCOL-1 ) = VALUE( 1:COPCOL )

*               Pad out the value with spaces.
                  PADCOL = PADCOL + NPAD

*               Add trailing blank.
                  FITSTR( PADCOL+1:PADCOL+1 ) = APS

*               Append remainder of the value.
                  FITSTR( PADCOL+2: ) = VALUE( COPCOL+1:70-NPAD )
               END IF

*         Other values are right justified but the leading spaces may
*         be omitted from the descriptor.  Therefore the FITS header
*         has to be reconstructed from the comment and the value.
            ELSE IF ( COMCOL .NE. 0 ) THEN

*            First extract the value and obtain its length.
               FVAL = VALUE( 1:COMCOL - 1 )
               LENVAL = CHR_LEN( FVAL )

*            Find the number of additional spaces over the usual one
*            between the value and the comment.
               EXTSPA = COMCOL - 1 - LENVAL

*            Form the comment part of the FITS header.  This assumes
*            at least one space between the value and the comment, as
*            is usual practice and recommended in the FITS standard.
               FITSTR( 31 + EXTSPA:80 ) = VALUE( COMCOL:70-EXTSPA )

*            Format the value.  Decide the data type of the value.
*            First try converting the value to floating point.  Note
*            that the values must be right justified.
               CALL CHR_CTOR( FVAL, RVALUE, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*                A numeric value.
                   FITSTR( 31-LENVAL:30 ) = FVAL( 1:LENVAL )

               ELSE

*                The error can be annulled since the error resulted from
*                a trial type conversion.
                   CALL ERR_ANNUL( STATUS )

*                Next try a logical value.
                   CALL CHR_CTOL( FVAL, LVALUE, STATUS )
                   IF ( STATUS .EQ. SAI__OK ) THEN

*                  A logical value.
                     FITSTR( 31-LENVAL:30 ) = FVAL( 1:LENVAL )

                  ELSE

*                   The error can be annulled since the error resulted
*                   from a trial type conversion.
                      CALL ERR_ANNUL( STATUS )

*                   Record a truncation.
                      IF ( LENVAL .GT. 20 ) LOST = .TRUE.

*                  By elimination it must be a character value.
*                  Character values may fill the card, so the value has
*                  been truncated try to put in as many characters as
*                  possible.  Switch off the truncation flag to prevent
*                  the value being overwritten by a warning comment.
*                  The minimum character length in a FITS header is 8.
*                  Character values are right justified.
                     LENVAL = MAX( 8, LENVAL )
                     IF ( LOST ) THEN
                        LOST = .FALSE.
                        IF ( LENVAL .GT. 65 )THEN
                           LENVAL = 65

*                        Write the value part of the card image, using
*                        ellipsis to indicate truncation.
                           FITSTR( 11:LENVAL+15 ) = APS/
     :                                              /FVAL( 1:LENVAL )/
     :                                              /'...'//APS
                        ELSE

*                        Write the value part of the card image.
                           FITSTR( 11:LENVAL+12 ) = APS/
     :                                              /FVAL( 1:LENVAL )/
     :                                              /APS
                        END IF
                     ELSE

*                     Write the value part of the card image.
                        FITSTR( 11:LENVAL+12 ) = APS//FVAL( 1:LENVAL )/
     :                                           /APS
                     END IF
                  END IF
               END IF

*            Release the error context.
               CALL ERR_RLSE


            ELSE

*            Record a truncation.  Values are truncated to 20
*            characters in FITS default style.
               LOST = .FALSE.
               IF ( LENVAL .GT. 20 ) THEN
                  LENVAL = 20
                  LOST = .TRUE.
               END IF

*            Start a new error context.
               CALL ERR_MARK

*            Decide the data type of the value.  First try converting
*            the value to floating point.  Note that the values must be
*            right justified.
               CALL CHR_CTOR( VALUE, RVALUE, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*                A numeric value.
                   FITSTR( 31-LENVAL:30 ) = VALUE( 1:LENVAL )

               ELSE

*                The error can be annulled since the error resulted from
*                a trial type conversion.
                   CALL ERR_ANNUL( STATUS )

*                Next try a logical value.
                   CALL CHR_CTOL( VALUE, LVALUE, STATUS )
                   IF ( STATUS .EQ. SAI__OK ) THEN

*                  A logical value.
                     FITSTR( 31-LENVAL:30 ) = VALUE( 1:LENVAL )

                  ELSE

*                   The error can be annulled since the error resulted
*                   from a trial type conversion.
                      CALL ERR_ANNUL( STATUS )

*                  By elimination it must be a character value.
*                  Character values may fill the card, so the value has
*                  been truncated try to put in as many characters as
*                  possible.  Switch off the truncation flag to prevent
*                  the value being overwritten by a warning comment.
*                  The minimum character length in a FITS header is 8.
*                  Character values are right justified.
                     LENVAL = MAX( 8, CHR_LEN( VALUE ) )
                     IF ( LOST ) THEN
                        LOST = .FALSE.
                        IF ( LENVAL .GT. 65 )THEN
                           LENVAL = 65

*                        Write the value part of the card image, using
*                        ellipsis to indicate truncation.
                          FITSTR( 11:LENVAL+15 ) = APS/
     :                                             /VALUE( 1:LENVAL )/
     :                                             /'...'//APS
                        ELSE

*                        Write the value part of the card image.
                           FITSTR( 11:LENVAL+12 ) = APS/
     :                                              /VALUE( 1:LENVAL )/
     :                                              /APS
                        END IF
                     ELSE

*                     Write the value part of the card image.
                        FITSTR( 11:LENVAL+12 ) = APS//VALUE( 1:LENVAL )/
     :                                           /APS
                     END IF
                  END IF
               END IF

*            Release the error context.
               CALL ERR_RLSE

*            If descriptor too long for available field note this fact
*            in the comment field.
               IF ( LOST ) THEN
                  FITSTR(31:32) =  '/ '
                  FITSTR(33:70) =  'Field width exceeded: value '/
     :                             /'truncated'
               END IF
            END IF

*         Get locator to appropriate element in .MORE.FITS structure.
            I = I + 1
            CALL DAT_CELL( FLOC, 1, I, FLOCI, STATUS )

*         Write the FITS card image to .MORE.FITS
            CALL DAT_PUT0C( FLOCI, FITSTR, STATUS )

*         Check for LABEL, BUNITS, and TITLE.
            IF ( INDEX( DESCR, 'LABEL' ) .GT. 0 ) THEN
               LABEL = VALUE
            ELSE IF ( INDEX( DESCR, 'TITLE' ) .GT. 0 ) THEN
               TITLE = VALUE
            ELSE IF ( INDEX( DESCR, 'BUNITS' ) .GT. 0 ) THEN
               UNITS = VALUE
            END IF

*         Convert descriptor name to uppercase for comparisons.
            CALL CHR_UCASE( DESCR )

*         Check for axis information, that is, CRVAL1, CDELT1 etc.
            DO IAX = 1, NDIM

*            Generate the keyword names.
               CALL CHR_ITOC( IAX, C, NCHAR )
               TEST1 = 'CRVAL'//C
               TEST2 = 'CDELT'//C
               TEST3 = 'CRPIX'//C
               TEST4 = 'CRTYPE'//C
               TEST5 = 'CROTA'//C

*            Test for the CRVALn keyword.  If present convert it to real
*            and record that it was found.
               IF ( INDEX( DESCR, TEST1 ) .GT. 0 ) THEN
                  CALL CHR_CTOR( VALUE, RVALUE, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     CRVAL( IAX ) = RVALUE
                     CRTHER( IAX ) = .TRUE.
                  END IF
               END IF

*            Test for the CDELTn keyword.  If present convert it to real
*            and record that it was found.
               IF ( INDEX( DESCR, TEST2 ) .GT. 0 ) THEN
                  CALL CHR_CTOR( VALUE, RVALUE, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     CDELT( IAX ) = RVALUE
                     CDTHER( IAX ) = .TRUE.
                  END IF
               END IF

*            Test for the CRPIXn keyword.  If present convert it to
*            real.
               IF ( INDEX( DESCR, TEST3 ) .GT. 0 ) THEN
                  CALL CHR_CTOR( VALUE, RVALUE, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) CRPIX( IAX ) = RVALUE
               END IF

*            Test for the CRTYPEn keyword.
               IF ( INDEX( DESCR, TEST4 ) .GT. 0 ) CRTYPE( IAX ) = VALUE

*            Test for the CROTAn keyword.  If present convert it to real
*            and record that it was found.
               IF ( INDEX( DESCR, TEST5 ) .GT. 0 ) THEN
                  CALL CHR_CTOR( VALUE, RVALUE, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     NOROT( IAX ) = ABS( RVALUE ) .LT. VAL__EPSR
                  END IF
               END IF

            END DO

*      End of the test for special descriptors.
         END IF

      END DO

998   CONTINUE

*   Set AXTHER(n) depending on whether or not there is sufficient
*   information to generate AXIS(n), and provided there is no axis
*   rotation (as this is not supported by the NDF in a standard way).
*   Also have to reinitialise the axis to pixel co-ordinates when
*   there is no or partial axis-centre information, or when there is
*   a rotated axis.
      DO I = 1, NDIM
         IF ( CDTHER( I ) .AND. CRTHER( I ) .AND. NOROT( I ) ) THEN
            AXTHER( I ) = .TRUE.
         ELSE
            AXTHER( I ) = .FALSE.
            CRVAL( I ) = 0.5
            CDELT( I ) = 1.0
         END IF
      END DO

999   CONTINUE

*   Truncate the allocated FITS structure if there were any arrays of
*   descriptors.
      IF ( N .GT. KOUNT ) CALL DAT_ALTER ( FLOC, 1, KOUNT, STATUS )

*   Annul the HDS locators.
      CALL DAT_ANNUL( FLOCI, STATUS )

      END
