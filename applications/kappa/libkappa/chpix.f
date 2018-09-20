      SUBROUTINE CHPIX( STATUS )
*+
*  Name:
*     CHPIX

*  Purpose:
*     Replaces the values of selected pixels in an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CHPIX( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application replaces selected elements of an NDF array
*     component with specified values.

*     Two methods are available for obtaining the regions and
*     replacement values, selected through Parameter MODE:
*
*     - from the parameter system (see Parameters SECTION and NEWVAL).
*     The task loops until there are no more elements to change,
*     indicated by a null value in response to a prompt.  For
*     non-interactive processing, supply the value of Parameter NEWVAL
*     on the command line.
*
*     - The second approach uses a text file, that is especially
*     beneficial where there are too many section and value pairs to
*     enter manually.  The file should contain two space-separated
*     columns; the first column is the NDF section to replace, and the
*     second supplies the value to insert into the section (see
*     Parameter FILE).

*  Usage:
*     chpix in out section newval [comp]

*  ADAM Parameters:
*     COMP = LITERAL (Read)
*        The name of the NDF array component to be modified.  The
*        options are: "Data", "Error", "Quality" or "Variance".
*        "Error" is the alternative to "Variance" and causes the
*        square of the supplied replacement value to be stored in the
*        output VARIANCE array. ["Data"]
*     FILE = FILENAME (Read)
*        Name of a text file containing the sections and replacement
*        values.  It is only accessed if Parameter MODE is given the
*        value "File".  Each line should contain an NDF section of
*        a region (see Parameter SECTION), then one or more spaces,
*        followed by the replacement value.  The value is either
*        numeric or "Bad", the latter requests that the bad value
*        be inserted into the section.  The file may contain comment
*        lines with the first character # or !.
*     IN = NDF (Read)
*        NDF structure containing the array component to be modified.
*     MODE = LITERAL (Read)
*        The mode in which the sections and replacement values are to be
*        obtained.  The supplied string can be one of the following values.
*
*        - "Interface" -- sections and values are obtained via Parameters
*                         SECTION and NEWVAL respectively.
*
*        - "File"      -- sections and values are obtained from a text file
*                         specified by Parameter FILE.
*
*        ["Interface"]
*     NEWVAL = LITERAL (Read)
*        Value to substitute in the output array element or elements.
*        The range of allowed values depends on the data type of the
*        array being modified.  NEWVAL="Bad" instructs that the bad
*        value appropriate for the array data type be substituted.
*        Placing NEWVAL on the command line permits only one section
*        to be replaced.  If there are multiple replacements, a null
*        value (!) terminates the loop. If the section being modified
*        contains only a single pixel, then the original value of that
*        pixel is used as the suggested default value.
*     OLDVAL = LITERAL (Write)
*        If the section being modified contains only a single pixel, then
*        the original value of that pixel is written out to this output
*        parameter.
*     OUT = NDF (Write)
*        Output NDF structure containing the modified version of
*        the array component.
*     SECTION = LITERAL (Read)
*        The elements to change.  This is defined as an NDF section, so
*        that ranges can be defined along any axis, and be given as
*        pixel indices or axis (data) co-ordinates.  So for example
*        "3,4,5" would select the pixel at (3,4,5); "3:5," would
*        replace all elements in Columns 3 to 5; ",4" replaces Line 4.
*        See "NDF sections" in SUN/95, or the online documentation for
*        details.  A null value (!) terminates the loop during multiple
*        replacements.
*     TITLE = LITERAL (Read)
*        Title for the output NDF structure.  A null value (!)
*        propagates the title from the input NDF to the output NDF. [!]

*  Examples:
*     chpix rawspec spectrum 55 100
*        Assigns the value 100 to the pixel at index 55 within the
*        one-dimensional NDF called rawspec, creating the output NDF
*        called spectrum.
*     chpix rawspec spectrum 10:19 0 error
*        Assigns the value 0 to the error values at indices 10 to 19
*        within the one-dimensional NDF called rawspec, creating the
*        output NDF called spectrum.  The rawspec dataset must have a
*        variance compoenent.
*     chpix in=rawimage out=galaxy section="~20,100:109" newval=bad
*        Assigns the bad value to the pixels in the section ~20,100:109
*        within the two-dimensional NDF called rawimage, creating the
*        output NDF called galaxy.  This section is the central
*        20 pixels along the first axis, and pixels 110 to 199 along the
*        second.
*     chpix in=zzcha out=zzcha_c section="45,21," newval=-1
*        Assigns value -1 to the pixels at index (45,21) within all
*        planes of the three-dimensional NDF called zzcha, creating
*        the output NDF called zzcha_c.
*     chpix harpcube harpmasked mode=file file=badbaseline.txt
*        This reads the text file called badbaseline.txt to obtain the
*        editing commands to be applied to the NDF called harpcube to
*        form the NDF harpmasked.

*  Related Applications:
*     KAPPA: ARDMASK, FILLBAD, GLITCH, NOMAGIC, REGIONMASK, SEGMENT,
*     SETMAGIC SUBSTITUTE, ZAPLIN; Figaro: CSET, ICSET, NCSET, TIPPEX.

*  Implementation Status:
*     -  The routine correctly processes the AXIS, DATA, QUALITY, LABEL,
*     TITLE, UNITS, HISTORY, WCS and VARIANCE components of an NDF; and
*     propagates all extensions.  Bad pixels and all non-complex
*     numeric data types can be handled.
*     -  The HISTORY component, if present, is simply propagated without
*     change.

*  Copyright:
*     Copyright (C) 1995, 1998, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2005 Particle Physics & Astronomy
*     Research Council.  Copyright (C) 2012 Science & Facilities
*     Research Council.  All Rights Reserved.

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
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     PWD: Peter W. Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1995 April 25 (MJC):
*        Original NDF version.
*     27-FEB-1998 (DSB):
*        Use NUM_ functions to convert lower precision integer values to
*        _INTEGER when calling PAR_MIX0I.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     01-OCT-2004 (PWD):
*        Moved CNF_PAR into declarations.
*     9-DEC-2005 (PWD):
*        Provide current pixel value as dynamic default for NEWVAL if section
*        contains only 1 pixel.
*     18-SEP-2007 (DSB):
*        Correct formatting of bad values when using current pixel value as
*        dynamic default for NEWVAL.
*     2008 May 1 (MJC):
*        Fix bug affecting bad-value substitution for byte and word
*        data types.
*     2012 May 8 (MJC):
*        Add _INT64 support.
*     2018 September 19 (MJC):
*        Add Parameters MODE and FILE.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE definitions
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants
      INCLUDE 'PAR_PAR'          ! PAR__ constants
      INCLUDE 'PAR_ERR'          ! PAR__ error constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'CNF_PAR'          ! CNF functions

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      INTEGER ACTVAL             ! State of parameter NEWVAL
      LOGICAL BADI               ! Input array may have bad values?
      LOGICAL BADO               ! Output array may have bad values?
      BYTE BVALUE                ! Replacement value for byte data
      CHARACTER * ( VAL__SZD ) CVALUE ! Replacement value as obtained
      CHARACTER * ( 8 ) COMP     ! Name of array component to analyse
      DOUBLE PRECISION DVALUE    ! Replacement value for d.p. data
      CHARACTER * ( 40 ) SUGDEF  ! Suggested default
      INTEGER EL                 ! Number of array elements mapped
      INTEGER FD                 ! Descriptor of the text file
      LOGICAL FILE               ! File mode was selected
      INTEGER IGRPS              ! Group of sections from text file
      INTEGER IGRPV              ! Group of valus from text file
      LOGICAL INTERF             ! Interface mode was selected
      INTEGER IVALUE             ! Replacement value for integer data
      INTEGER*8 KVALUE           ! Replacement value for 64-bit integer
      CHARACTER * ( DAT__SZLOC ) LOC ! Locators for the NDF
      LOGICAL LOOP               ! Loop for another section to replace
      CHARACTER * ( 8 ) MCOMP    ! Component name for mapping arrays
      INTEGER MEMBER             ! Number of editing lines
      CHARACTER * 9 MODE         ! Mode for getting sections and values
      INTEGER NCHAR              ! Number of characters written to string
      INTEGER NCSECT             ! Number of characters in section
      INTEGER NDFI               ! Identifier for input NDF
      INTEGER NDFO               ! Identifier for output NDF
      INTEGER NDFS               ! Identifier for NDF section
      INTEGER NEDIT              ! Number of in-file edits
      LOGICAL OPEN               ! Is text file open?
      REAL RVALUE                ! Replacement value for real data
      INTEGER PNTR( 1 )          ! Pointer to mapped NDF array
      CHARACTER * ( 80 ) SECT    ! Section specifier
      LOGICAL THERE              ! Array component exists?
      CHARACTER * ( NDF__SZTYP ) TYPE ! Numeric type for processing
      INTEGER * 2 WVALUE         ! Replacement value for word data

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the mode by which values and sections are accessed.
      CALL PAR_CHOIC( 'MODE', 'Interface', 'Interface,File', .TRUE.,
     :                MODE, STATUS )
      FILE = MODE .EQ. 'FILE'
      INTERF = MODE .EQ. 'INTERFACE'

      OPEN = .FALSE.
      IF ( FILE ) THEN

*  Open the text file of editing instructions.
         CALL FIO_ASSOC( 'FILE', 'READ', 'LIST', 0, FD, STATUS )
         IF ( STATUS .EQ. SAI__OK ) OPEN = .TRUE.

* Create groups for the sections and values supplied in the text file.
         CALL GRP_NEW( 'Sections', IGRPS, STATUS )
         CALL GRP_NEW( 'Values', IGRPV, STATUS )

*  Read the values and sections into the groups.  Ignore blank and
*  comment lines.
         CALL KPS1_CHPIN( FD, '#,!', IGRPS, IGRPV, MEMBER, STATUS )
         CALL MSG_SETI( 'N', MEMBER )
         CALL MSG_OUTIF( MSG__VERB, '', 'Number sections read is ^N',
     :                   STATUS )

      END IF

      NEDIT = 0

*  Obtain the input NDF.
*  =====================

*  Start a new NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDF.
      CALL LPG_ASSOC( 'IN', 'Read', NDFI, STATUS )

*  Determine which array component is to be modified.
      CALL PAR_CHOIC( 'COMP', 'Data', 'Data,Error,Quality,Variance',
     :                .FALSE., COMP, STATUS )

*  Most NDF routines with a component argument don't recognise 'ERROR',
*  so we need two variables.  Thus convert 'ERROR' into 'VARIANCE' in
*  the variable needed for such routines.  The original value is held
*  in a variable with the prefix M for mapping, as one of the few
*  routines that does support 'ERROR' is NDF_MAP.
      MCOMP = COMP
      IF ( COMP .EQ. 'ERROR' ) COMP = 'VARIANCE'

*  Check that the required component exists and report an error if it
*  does not.
      CALL NDF_STATE( NDFI, COMP, THERE, STATUS )
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( .NOT. THERE ) ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'COMP', MCOMP )
         CALL NDF_MSG( 'NDF', NDFI )
         CALL ERR_REP( 'STATS_NOCOMP',
     :                 'The ^COMP component is undefined in the NDF ' //
     :                 'structure ^NDF', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the numeric type of the NDF array component to be analysed.
      CALL NDF_TYPE( NDFI, COMP, TYPE, STATUS )

*  Create the output NDF.
*  ======================
*  Obtain an output NDF and propagate the whole of the input NDF to it.
      CALL LPG_PROP( NDFI, 'WCS,Data,Variance,Quality,Axis,Units',
     :               'OUT', NDFO, STATUS )

*  Get the title for the output NDF.
      CALL NDF_CINP( 'TITLE', NDFO, 'Title', STATUS )

*  Obtain a locator to the output NDF.
      CALL NDF_LOC( NDFO, 'Update', LOC, STATUS )

*  Initialise flag indicating whether or not there are bad values in
*  the array.  Find out if there are currently no bad values.  If there
*  may already bad values, the bad-pixel flag is already appropriate
*  whatever is substituted.  If there are no bad values, we must record
*  any bad values that are filled into the array, and hence whether or
*  not to switch the bad-pixel flag.  Of course, the values may be
*  overwritten with good values, but the flag indicates only that bad
*  values _may_ be present.
      CALL NDF_BAD( NDFO, COMP, .FALSE., BADI, STATUS )
      BADO = .FALSE.

*  Main loop.
*  ==========

*  Determine whether or not to loop.  Looping does not occur if the
*  NEWVAL is given on the command line, i.e. it is already in the active
*  state.
      IF ( INTERF ) CALL LPG_STATE( 'NEWVAL', ACTVAL, STATUS )

      LOOP = .TRUE.
      DO WHILE ( STATUS .EQ. SAI__OK .AND. LOOP )
         NEDIT = NEDIT + 1

*  Do not loop if the value was given on the command line.
         LOOP = ( INTERF .AND. ACTVAL .NE. PAR__ACTIVE ) .OR.
     :          ( FILE .AND. NEDIT .LT. MEMBER )

*  Obtain the section string. and the value in file mode.
         IF ( INTERF ) THEN
            CALL PAR_GET0C( 'SECTION', SECT, STATUS )
         ELSE IF ( FILE ) THEN
            CALL GRP_GET( IGRPS, NEDIT, 1, SECT, STATUS )
            CALL GRP_GET( IGRPV, NEDIT, 1, CVALUE, STATUS )
         END IF
         NCSECT = CHR_LEN( SECT )

*  Create the section in the output array.
         CALL NDF_FIND( LOC, '(' // SECT( :NCSECT ) // ')', NDFS,
     :                  STATUS )

*  Map the array component of the section.
         CALL KPG1_MAP( NDFS, MCOMP, TYPE, 'Update', PNTR, EL, STATUS )

*  Perform the replacements.
*  =========================
*
*  This is merely done by merely filling the section with the constant.
*  The replacement value and filling routine are type dependent, so call
*  the appropriate data type.  A variable must be passed with the
*  non-numeric default to circumvent a compiler or CHR bug.

*  Real
*  ----
         SUGDEF = 'Junk'
         IF ( TYPE .EQ. '_REAL' ) THEN

*  Drive a suitable suggested default.  If the section contains only a
*  single pixel, use its value as the default for the parameter.
            IF ( INTERF ) THEN
               IF ( EL .EQ. 1 ) THEN
                  CALL KPG1_MEANR( 1, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                             RVALUE, STATUS )
                  IF ( RVALUE .NE. VAL__BADR ) THEN
                     CALL CHR_RTOC( RVALUE, SUGDEF, NCHAR )
                  ELSE
                     SUGDEF = 'Bad'
                  END IF
                  CALL PAR_PUT0C( 'OLDVAL', SUGDEF, STATUS )
               END IF

*  Get the replacement value.  The range depends on the processing data
*  type.
               CALL PAR_MIX0R( 'NEWVAL', SUGDEF, VAL__MINR, VAL__MAXR,
     :                         'Bad', .FALSE., CVALUE, STATUS )
            END IF

*  Convert the value string to a numerical value.
            IF ( CVALUE .EQ. 'BAD' ) THEN
               RVALUE = VAL__BADR
               BADO = .TRUE.
            ELSE
               CALL CHR_CTOR( CVALUE, RVALUE, STATUS )
            END IF

*  Fill the array with the constant.
            CALL KPG1_FILLR( RVALUE, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                       STATUS )

*  Double precision
*  ----------------
         ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
            IF ( INTERF ) THEN

*  Drive a suitable suggested default.  If the section contains only a
*  single pixel, use its value as the default for the parameter.
               IF ( EL .EQ. 1 ) THEN
                  CALL KPG1_MEAND( 1, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                             DVALUE, STATUS )
                  IF ( DVALUE .NE. VAL__BADD ) THEN
                     CALL CHR_DTOC( DVALUE, SUGDEF, NCHAR )
                  ELSE
                     SUGDEF = 'Bad'
                  END IF
                  CALL PAR_PUT0C( 'OLDVAL', SUGDEF, STATUS )
               END IF

*  Get the replacement value.
               CALL PAR_MIX0D( 'NEWVAL', SUGDEF, VAL__MIND, VAL__MAXD,
     :                         'Bad', .FALSE., CVALUE, STATUS )
            END IF

*  Convert the value string to a numerical value.
            IF ( CVALUE .EQ. 'BAD' ) THEN
               DVALUE = VAL__BADD
               BADO = .TRUE.
            ELSE
               CALL CHR_CTOD( CVALUE, DVALUE, STATUS )
            END IF

*  Fill the array with the constant.
            CALL KPG1_FILLD( DVALUE, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                       STATUS )

*  Integer
*  -------
         ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
            IF ( INTERF ) THEN

*  Drive a suitable suggested default.  If the section contains only a
*  single pixel, use its value as the default for the parameter.
               IF ( EL .EQ. 1 ) THEN
                  CALL KPG1_MEANI( 1, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                             IVALUE, STATUS )
                  IF ( IVALUE .NE. VAL__BADI ) THEN
                     CALL CHR_ITOC( IVALUE, SUGDEF, NCHAR )
                  ELSE
                     SUGDEF = 'Bad'
                  END IF
                  CALL PAR_PUT0C( 'OLDVAL', SUGDEF, STATUS )
               END IF

*  Get the replacement value.
               CALL PAR_MIX0I( 'NEWVAL', SUGDEF, VAL__MINI, VAL__MAXI,
     :                         'Bad', .FALSE., CVALUE, STATUS )
            END IF

*  Convert the value string to a numerical value.
            IF ( CVALUE .EQ. 'BAD' ) THEN
               IVALUE = VAL__BADI
               BADO = .TRUE.
            ELSE
               CALL CHR_CTOI( CVALUE, IVALUE, STATUS )
            END IF

*  Fill the array with the constant.
            CALL KPG1_FILLI( IVALUE, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                       STATUS )

*  64-bit Integer
*  --------------
         ELSE IF ( TYPE .EQ. '_INT64' ) THEN
            IF ( INTERF ) THEN

*  Drive a suitable suggested default.  If the section contains only a
*  single pixel, use its value as the default for the parameter.
               IF ( EL .EQ. 1 ) THEN
                  CALL KPG1_MEANK( 1, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                             KVALUE, STATUS )
                  IF ( KVALUE .NE. VAL__BADK ) THEN
                     CALL CHR_ITOC( KVALUE, SUGDEF, NCHAR )
                  ELSE
                     SUGDEF = 'Bad'
                  END IF
                  CALL PAR_PUT0C( 'OLDVAL', SUGDEF, STATUS )
               END IF

*  Get the replacement value.
               CALL PAR_MIX0I( 'NEWVAL', SUGDEF, VAL__MINK, VAL__MAXK,
     :                         'Bad', .FALSE., CVALUE, STATUS )
            END IF

*  Convert the value string to a numerical value.
            IF ( CVALUE .EQ. 'BAD' ) THEN
               KVALUE = VAL__BADK
               BADO = .TRUE.
            ELSE
               CALL CHR_CTOK( CVALUE, KVALUE, STATUS )
            END IF

*  Fill the array with the constant.
            CALL KPG1_FILLK( KVALUE, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                       STATUS )

*  Byte
*  ----
         ELSE IF ( TYPE .EQ. '_BYTE' ) THEN
            IF ( INTERF ) THEN

*  Drive a suitable suggested default.  If the section contains only a
*  single pixel, use its value as the default for the parameter.
               IF ( EL .EQ. 1 ) THEN
                  CALL KPG1_MEANB( 1, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                             BVALUE, STATUS )
                  IF ( BVALUE .NE. VAL__BADB ) THEN
                     CALL CHR_ITOC( NUM_BTOI( BVALUE ), SUGDEF, NCHAR )
                  ELSE
                     SUGDEF = 'Bad'
                  END IF

                  CALL PAR_PUT0C( 'OLDVAL', SUGDEF, STATUS )
               END IF

*  Get the replacement value.
               CALL PAR_MIX0I( 'NEWVAL', SUGDEF, NUM_BTOI( VAL__MINB ),
     :                         NUM_BTOI( VAL__MAXB ), 'Bad', .FALSE.,
     :                         CVALUE, STATUS )
            END IF

*  Convert the value string to a numerical value.
            IF ( CVALUE .EQ. 'BAD' ) THEN
               BVALUE = VAL__BADB
               BADO = .TRUE.
            ELSE
               CALL CHR_CTOI( CVALUE, IVALUE, STATUS )
               BVALUE = NUM_ITOB( IVALUE )
            END IF

*  Fill the array with the constant.
            CALL KPG1_FILLB( BVALUE, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                       STATUS )

*  Unsigned Byte
*  -------------
         ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
            IF ( INTERF ) THEN

*  Drive a suitable suggested default.  If the section contains only a
*  single pixel, use its value as the default for the parameter.
               IF ( EL .EQ. 1 ) THEN
                  CALL KPG1_MEANUB( 1, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                              BVALUE, STATUS )
                  IF ( BVALUE .NE. VAL__BADUB ) THEN
                     CALL CHR_ITOC( NUM_UBTOI( BVALUE ), SUGDEF, NCHAR )
                  ELSE
                     SUGDEF = 'Bad'
                  END IF
                  CALL PAR_PUT0C( 'OLDVAL', SUGDEF, STATUS )
               END IF

*  Get the replacement value.
               CALL PAR_MIX0I( 'NEWVAL', SUGDEF,
     :                         NUM_UBTOI( VAL__MINUB ),
     :                         NUM_UBTOI( VAL__MAXUB ), 'Bad', .FALSE.,
     :                         CVALUE, STATUS )
            END IF

*  Convert the value string to a numerical value.
            IF ( CVALUE .EQ. 'BAD' ) THEN
               BVALUE = VAL__BADUB
               BADO = .TRUE.
            ELSE
               CALL CHR_CTOI( CVALUE, IVALUE, STATUS )
               BVALUE = NUM_ITOUB( IVALUE )
            END IF

*  Fill the array with the constant.
            CALL KPG1_FILLUB( BVALUE, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                        STATUS )

*  Word
*  ----
         ELSE IF ( TYPE .EQ. '_WORD' ) THEN
            IF ( INTERF ) THEN

*  Drive a suitable suggested default.  If the section contains only a
*  single pixel, use its value as the default for the parameter.
               IF ( EL .EQ. 1 ) THEN
                  CALL KPG1_MEANW( 1, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                             WVALUE, STATUS )
                  IF ( WVALUE .NE. VAL__BADW ) THEN
                     CALL CHR_ITOC( NUM_WTOI( WVALUE ), SUGDEF, NCHAR )
                  ELSE
                     SUGDEF = 'Bad'
                  END IF
                  CALL PAR_PUT0C( 'OLDVAL', SUGDEF, STATUS )
               END IF

*  Get the replacement value.
               CALL PAR_MIX0I( 'NEWVAL', SUGDEF, NUM_WTOI( VAL__MINW ),
     :                          NUM_WTOI( VAL__MAXW ), 'Bad', .FALSE.,
     :                          CVALUE, STATUS )
            END IF

*  Convert the value string to a numerical value.
            IF ( CVALUE .EQ. 'BAD' ) THEN
               WVALUE = VAL__BADW
               BADO = .TRUE.
            ELSE
               CALL CHR_CTOI( CVALUE, IVALUE, STATUS )
               WVALUE = NUM_ITOW( IVALUE )
            END IF

*  Fill the array with the constant.
            CALL KPG1_FILLW( WVALUE, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                       STATUS )

*  Unsigned Word
*  -------------
         ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
            IF ( INTERF ) THEN

*  Drive a suitable suggested default.  If the section contains only a
*  single pixel, use its value as the default for the parameter.
               IF ( EL .EQ. 1 ) THEN
                  CALL KPG1_MEANUW( 1, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                              WVALUE, STATUS )
                  IF ( WVALUE .NE. VAL__BADUW ) THEN
                     CALL CHR_ITOC( NUM_UWTOI( WVALUE ), SUGDEF, NCHAR )
                  ELSE
                     SUGDEF = 'Bad'
                  END IF
                  CALL PAR_PUT0C( 'OLDVAL', SUGDEF, STATUS )
               END IF

*  Get the replacement value.
               CALL PAR_MIX0I( 'NEWVAL', SUGDEF,
     :                         NUM_UWTOI( VAL__MINUW ),
     :                         NUM_UWTOI( VAL__MAXUW ), 'Bad', .FALSE.,
     :                         CVALUE, STATUS )
            END IF

*  Convert the value string to a numerical value.
            IF ( CVALUE .EQ. 'BAD' ) THEN
               WVALUE = VAL__BADUW
               BADO = .TRUE.
            ELSE
               CALL CHR_CTOI( CVALUE, IVALUE, STATUS )
               WVALUE = NUM_ITOUW( IVALUE )
            END IF

*  Fill the array with the constant.
            CALL KPG1_FILLUW( WVALUE, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                        STATUS )
         END IF

*  Unmap and annul the section so that it gets written back to the
*  output NDF array.
         CALL NDF_ANNUL( NDFS, STATUS )

*  Annul a null status as this is expected, and closes the loop.
         IF ( INTERF ) THEN
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               LOOP = .FALSE.

*  Cancel the previous values of NEWVAL and SECTION for the loop.
            ELSE IF ( LOOP .AND. STATUS .EQ. SAI__OK ) THEN
                CALL PAR_CANCL( 'SECTION', STATUS )
                CALL PAR_CANCL( 'NEWVAL', STATUS )
            END IF
         END IF

*  End of the do-while loop for the section.
      END DO

*  Set the bad-pixel flag if it previously indicated no bad values,
*  but some have been introduced.  There is no flag for the QUALITY
*  component.
      IF ( .NOT. BADI .AND. BADO .AND. COMP .NE. 'QUALITY' )
     :  CALL NDF_SBAD( BADO, NDFO, COMP, STATUS )

*  Closedown sequence.
*  ===================
  999 CONTINUE

*  Close an open text file.
      IF ( OPEN ) CALL FIO_CLOSE( FD, STATUS )

*  Delete the groups.
      IF ( FILE ) THEN
         CALL GRP_DELET( IGRPS, STATUS )
         CALL GRP_DELET( IGRPV, STATUS )
      END IF

*  Free NDF resources.
      CALL NDF_END( STATUS )

*  Write the closing error report if something has gone wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CHPIX_ERR',
     :     'CHPIX: Unable to change array values in an NDF.', STATUS )
      END IF

      END
