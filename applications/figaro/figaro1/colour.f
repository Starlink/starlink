      SUBROUTINE COLOUR
*+
*  Name:
*     COLOUR

*  Purpose:
*     Set colour table for image display.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Figaro application

*  Invocation:
*     CALL COLOUR

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine sets the colour table of the image display device. It
*     can either be reset to a grey scale, or an RGB lookup table from a
*     3xN image can be used. The lookup table must have numbers between
*     0.0 and 1.0.

*  Usage:
*     colour table

*  ADAM Parameters:
*     TABLE = _CHAR (Read)
*        The colour table file to be used. The programme will look for
*        this file in the default directory and then in the standard
*        Figaro directories. If TABLE is 'grey' or 'gray' this is
*        trapped and a grey scale table is set up.
*     IDEV = _CHAR (Read)
*        The name of the imaging device, normally got from a global
*        parameter which was set with the IDEV command.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     mjcl: Martin Clayton (Starlink, UCL)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12 Nov 1984 (ks):
*        Original version.
*     13 Sep 1988 (ks):
*        Now uses the standard TVPCKG routines. Also recognises 'GREY'
*        in lower case, and uses DSA routines to get the logical unit
*        used.
*     10 Mar 1993 (hme):
*        Conversion to PGPLOT graphics and NDF lookup tables. Use IDEV
*        device.
*     16 Aug 1993 (hme):
*        Take care of version numbers in VAX/VMS file names.
*     26 Jul 1996 (mjcl):
*        Added check for ABORT repsponse to TABLE prompt.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Local Constants:
      INTEGER MAXPEN             ! Protect so many pens
      PARAMETER ( MAXPEN = 16 )

*  Local Variables:
      INTEGER DSASTA             ! DSA status
      INTEGER IDISTA             ! PGPLOT status
      INTEGER IGNORE             ! Ignored status
      INTEGER COL1               ! First PGPLOT pen
      INTEGER COL2               ! Last PGPLOT pen
      INTEGER I                  ! Loop index
      INTEGER UNIT               ! Fortran unit number
      INTEGER NDIM               ! LUT dimensionality
      INTEGER DIMS( 2 )          ! LUT dimensions
      INTEGER NELM               ! LUT size
      INTEGER PNTR               ! Pointer to LUT
      INTEGER SLOT               ! Slot to map LUT
      REAL MAXMIN                ! Pen range
      REAL GREY                  ! Grey intensity value
      REAL DMIN                  ! Minimum in LUT
      REAL DMAX                  ! Maximum in LUT
      CHARACTER * ( 32 ) DEVNAM  ! Image device name
      CHARACTER * ( 132 ) TABLE  ! LUT name
      CHARACTER * ( 8 ) UTABLE   ! Upper case LUT spec

*  Internal References:
      LOGICAL PAR_ABORT          ! (F)PAR abort flag
      INTEGER PGBEGIN            ! Open image device
      INTEGER ICH_FOLD           ! Fold string to upper case
      INTEGER ICH_LEN            ! Used length of a string

*.

*  Startup.
      DSASTA = 0
      CALL DSA_OPEN( DSASTA )
      IF ( DSASTA .NE. 0 ) GO TO 500

*  Open the image display.
      CALL VAR_GETCHR( 'IDEV', 0, 0, DEVNAM, DSASTA )
      IF ( DSASTA .NE. 0 ) THEN
         CALL PAR_WRUSER( 'IMAGE: Error: No display specified. ' //
     :      'Use IDEV to select an image display.', IGNORE )
         GO TO 500
      END IF
      IDISTA = PGBEGIN( 0, DEVNAM(:ICH_LEN(DEVNAM)) // '/append', 1, 1 )
      IF ( IDISTA .NE. 1 ) THEN
         CALL PAR_WRUSER( 'IMAGE: Error opening image display.' ,
     :      IGNORE )
         GO TO 500
      END IF
      CALL PGASK( .FALSE. )

*  Find out how many colours there are.
      CALL PGQCOL( COL1, COL2 )
      IF ( COL2 .LE. MAXPEN ) THEN
         CALL PAR_WRUSER( 'COLOUR: Error: Insufficient colours ' //
     :      'available for display.', IGNORE )
         GO TO 500
      END IF
      MAXMIN = FLOAT( COL2 - MAXPEN )

*  Find out about the colour table.
      CALL PAR_RDCHAR( 'TABLE', 'grey', TABLE )
      IF ( PAR_ABORT() ) GO TO 500
      UTABLE = TABLE
      IGNORE = ICH_FOLD( UTABLE )

*  If grey scale requested.
      IF ( UTABLE .EQ. 'GREY' .OR. UTABLE .EQ. 'GRAY' ) THEN
         DO 1 I = MAXPEN, COL2
            GREY = FLOAT( I - MAXPEN ) / MAXMIN
            CALL PGSCR( I, GREY, GREY, GREY )
 1       CONTINUE

*  Else (need a LUT in an NDF).
      ELSE

*     First look for the file in the standard list of directories. This
*     will open the file on a free Fortran unit. That is not much use,
*     but helps to find out its full name.
         CALL DSA_GET_LU( UNIT, DSASTA )
         CALL FIG_OPFILE( TABLE, 'sdf', UNIT, DSASTA )
         IF ( DSASTA .NE. 0 ) THEN
            CALL PAR_WRUSER( 'COLOUR: Error opening colour lookup ' //
     :         'table. File not found.', IGNORE )
            GO TO 500
         END IF

*     Now find out its name, close it and access it via DSA.
*     For a VAX we must disregard any version number in the file name.
*     Hence the testing for a semicolon.
*     On any machine we must disregard the file name extension ".sdf".
         INQUIRE ( UNIT = UNIT, NAME = TABLE )
         CLOSE ( UNIT = UNIT )
         CALL DSA_FREE_LU( UNIT, DSASTA )
         I = INDEX( TABLE, ';' )
         IF ( I .EQ. 0 ) THEN
            I = ICH_LEN(TABLE) - 4
         ELSE IF ( I .LE. 5 ) THEN
            CALL PAR_WRUSER( 'COLOUR: Error parsing colour table ' //
     :         'file name.', IGNORE )
            GO TO 500
         ELSE
            I = I - 5
         END IF
         CALL DSA_NAMED_INPUT( 'LUT', TABLE(:I), DSASTA )
         IF ( DSASTA .NE. 0 ) GO TO 500

*     Check shape, map data, check range.
         CALL DSA_DATA_SIZE( 'LUT', 2, NDIM, DIMS, NELM, DSASTA )
         IF ( DSASTA .NE. 0 ) GO TO 500
         IF ( NDIM .NE. 2 .OR. DIMS(1) .NE. 3 ) THEN
            CALL PAR_WRUSER( 'COLOUR: Error reading colour lookup ' //
     :         'table. Table must be 2D and 3 by N in size.', IGNORE )
            GO TO 500
         END IF
         CALL DSA_MAP_DATA( 'LUT', 'READ', 'FLOAT', PNTR, SLOT, DSASTA )
         IF ( DSASTA .NE. 0 ) GO TO 500
         CALL GEN_RANGEF( %VAL( CNF_PVAL(PNTR) ), 1, NELM, DMAX, DMIN )
         IF ( DMIN .LT. 0. .OR. DMAX .GT. 1.0 ) THEN
            CALL PAR_WRUSER( 'COLOUR: Error reading colour lookup ' //
     :         'table. Table values must be between 0.0 and 1.0.',
     :         IGNORE )
            GO TO 500
         END IF

*     All's well, now do the work.
         CALL FIG_COLOUR_1( DIMS(2), %VAL( CNF_PVAL(PNTR) ), MAXPEN,
     :                      COL2 )
      END IF

*  Tidy up.
 500  CONTINUE
      CALL PGEND
      CALL DSA_CLOSE( DSASTA )
      END
      SUBROUTINE FIG_COLOUR_1( DIM2, LUT, COLLO, COLHI )
*+
*  Name:
*     FIG_COLOUR_1

*  Purpose:
*     Pick PGPLOT colour table from 3 by N lookup table array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIG_COLOUR_1( DIM2, LUT, COLLO, COLHI )

*  Description:
*     This routine will load the colours for PGPLOT pens COLLO ...
*     COLHIO from a two-dimensional look up table. The lookup table must
*     have values between 0.0 and 1.0 and have first dimension 3 (i.e. 3
*     pixels along first axis, or three colour values per intensity).
*     The number of entries DIM2 in the lookup table need not match the
*     number of pens COLHI - COLLO + 1 to be set. For each pen to be set
*     the nearest entry in the lookup table will be chosen.

*  Arguments:
*     DIM2 = INTEGER (Given)
*        The number of entries in the colour table.
*     LUT( 3, DIM2 ) = REAL (Given)
*        The colour lookup table. The LUT(1...3,*) are the red, green
*        and blue values for any entry.
*     COLLO = INTEGER (Given)
*        The first PGPLOT pen to be set.
*     COLHI = INTEGER (Given)
*        The last PGPLOT pen to be set. COLLHI must be greater than
*        COLLO.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     09 Mar 1993 (HME):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER DIM2
      REAL LUT( 3, DIM2 )
      INTEGER COLLO
      INTEGER COLHI

*  Local Variables:
      INTEGER I, J               ! Loop indices
      REAL MAXMIN                ! Scaling between pens and LUT

*.

      MAXMIN = FLOAT( DIM2 - 1 ) / FLOAT( COLHI - COLLO )
      DO 1 J = COLLO, COLHI
         I = INT( FLOAT( J - COLLO ) * MAXMIN + 1.5 )
         CALL PGSCR( J, LUT(1,I), LUT(2,I), LUT(3,I) )
 1    CONTINUE

      END
