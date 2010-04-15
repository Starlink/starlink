      SUBROUTINE POL1_MKCAT( PARAM, IWCS, CIRC, UNITS, VAR, ANGROT,
     :                       TITLE, GETEQM, CI, EQMAP, NAME, STATUS )
*+
*  Name:
*     POL1_MKCAT

*  Purpose:
*     Create a CAT catalogue to hold polarisation parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_MKCAT( PARAM, IWCS, CIRC, UNITS, VAR, ANGROT, TITLE,
*                      GETEQM, CI, EQMAP, NAME, STATUS )

*  Description:
*     This routine creates a new CAT catalogue. Columns are created for the
*     grid coordinates (X, Y and optionally Z), Stokes parameters (I, (Q,U)
*     or (V) ), percentage  polarisation, polarised intensity, and
*     polarisation angle (in degrees).
*
*     Columns for the standard deviation associated with each (except pixel
*     coordinates) are also produced. For circular polarisation, the Q and
*     U columns are replaced by a single column for V.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter through which the name of the new catalogue should
*        be obtained.
*     IWCS = INTEGER (Given)
*        A FrameSet to define the names to give to the X, Y and Z columns.
*        The Base Frame is the 2D (or 3D) PIXEL Frame. On exit, a Frame with
*        Domain POLANAL is added to the FrameSet. The first axis in this
*        Frame defines the reference direction in the catalogue.
*     CIRC = LOGICAL (Given)
*        Set this to .TRUE. if circular polarisation is being measured.
*     UNITS = CHARACTER * ( * ) (Given)
*        The units used to measure intensities and Stokes parameters. May
*        be blank.
*     VAR = LOGICAL (Given)
*        Are variance values available? If not, the columns containing
*        standard deviations are not created.
*     ANGROT = REAL (Given)
*        ACW angle in degrees from pixel X axis to the reference direction
*        in the output catalogue.
*     TITLE = CHARACTER * ( * ) (Given)
*        A title string to store as the TITLE parameter for the catalogue.
*        No TITLE parameter is created if TITLE is blank.
*     GETEQM = LOGICAL (Given)
*        If TRUE, then EQMAP is returned holding the ( X,Y(,Z) )->(RA,DEC)
*        Mapping, if possible. Otherwise, GETEQM is returned holding
*        AST__NULL.
*     CI = INTEGER (Returned)
*        A CAT identifier for the created catalogue.
*     EQMAP = INTEGER (Returned)
*        An identifier for an AST Mapping describing the conversion of
*        (X,Y(,Z)) pixel co-ordinates to RA,DEC (J2000) co-ordinates. If the
*        supplied FrameSet (IWCS) does not allow this information to be
*        found, or if GETEQM is .FALSE., AST__NULL is returned for EQMAP.
*     NAME = CHARACTER * ( * ) (Returned)
*        The full file spec of the created catalogue.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-JAN-1998 (DSB):
*        Original version.
*     17-MAY-2000 (DSB):
*        Added creation of RA/DEC columns, and argument EQFS.
*     2-FEB- 2001 (DSB):
*        Added support for 4D Stokes cubes.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'CAT_PAR'          ! CAT constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER IWCS
      LOGICAL CIRC
      CHARACTER UNITS*(*)
      LOGICAL VAR
      REAL ANGROT
      CHARACTER TITLE*(*)
      LOGICAL GETEQM

*  Arguments Returned:
      INTEGER CI
      INTEGER EQMAP
      CHARACTER NAME*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER EQFS               ! An AST FrameSet
      INTEGER II                 ! CAT identifier for most recent part
      INTEGER FRM                ! Pointer to Base Frame
      INTEGER NAX                ! No. of PIXEL axes
      INTEGER QI                 ! Identifier for a catalogue parameter
      INTEGER TMPLT              ! Template Frame
      INTEGER IFRM               ! Frame index of SKY Frame within IWCS
      INTEGER LWCS               ! Local copy of WCS FrameSet
*.

      EQMAP = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the Symbols, etc, used to refer to axes 1, 2 and 3 of the Base Frame
*  in the supplied FrameSet.
      FRM = AST_GETFRAME( IWCS, AST__BASE, STATUS )

*  Find the number of axes.
      NAX = AST_GETI( FRM, 'NAXES', STATUS )

*  Create the catalogue.
      CALL CTG_CREA1( PARAM, CI, NAME, STATUS )

*  If required, set the TITLE parameter.
      IF( TITLE .NE. ' ' ) THEN
         CALL CAT_PPTSC( CI, 'TITLE', TITLE, 'Catalogue title', QI,
     :                   STATUS )
         CALL CAT_TRLSE( QI, STATUS )
      END IF

*  Create the columns...

*  Pixel X coordinate.
      CALL POL1_CNEWS( CI, 'X', .TRUE.,
     :                CAT__TYPER, 0, AST_GETC( FRM, 'Unit(1)', STATUS ),
     :                'F7.1', AST_GETC( FRM, 'Label(1)', STATUS ), II,
     :                STATUS )

*  Pixel Y coordinate.
      CALL POL1_CNEWS( CI, 'Y', .TRUE.,
     :                CAT__TYPER, 0, AST_GETC( FRM, 'Unit(2)', STATUS ),
     :                'F7.1', AST_GETC( FRM, 'Label(2)', STATUS ), II,
     :                STATUS )

*  Pixel Z coordinate (if present).
      IF( NAX .EQ. 3 ) THEN
         CALL POL1_CNEWS( CI, 'Z', .TRUE.,
     :                CAT__TYPER, 0, AST_GETC( FRM, 'Unit(3)', STATUS ),
     :                'F7.1', AST_GETC( FRM, 'Label(3)', STATUS ), II,
     :                STATUS )
      END IF

*  If we can get RA and DEC positions using the FrameSet, add RA and DEC
*  columns.
      IF( GETEQM ) THEN

*  Create a copy to avoid KPG1_ASFFR modifying the original FrameSet.
         LWCS = AST_COPY( IWCS, STATUS )

         CALL KPG1_ASFFR( LWCS, 'SKY', IFRM, STATUS )
         IF( IFRM .NE. AST__NOFRAME ) THEN

            TMPLT = AST_SKYFRAME( 'System=FK5,Equinox=J2000', STATUS )

            EQFS = AST_FINDFRAME( LWCS, TMPLT, ' ', STATUS )

            CALL AST_ANNUL( TMPLT, STATUS )
            IF( EQFS .NE. AST__NULL ) THEN

*  RA .
               CALL POL1_CNEWS( CI, 'RA', .TRUE., CAT__TYPED, 0,
     :                         'RADIANS{HOURS}', 'D16.8',
     :                         'Right Ascension (FK5)', II, STATUS )

*  DEC .
               CALL POL1_CNEWS( CI, 'DEC', .TRUE., CAT__TYPED, 0,
     :                         'RADIANS{DEGREES}', 'D16.8',
     :                         'Declination (FK5)', II, STATUS )

*  Parameters EPOCH and EQUINOX.
               CALL CAT_PPTSC( CI, 'EPOCH', AST_GETC( EQFS, 'Epoch',
     :                                                STATUS ),
     :                         'Epoch of observation', II, STATUS )
               CALL CAT_PPTSC( CI, 'EQUINOX', 'J2000', 'Epoch of '//
     :                         'reference equinox', II, STATUS )
               EQMAP = AST_GETMAPPING( EQFS, AST__BASE, AST__CURRENT,
     :                                 STATUS )
               CALL AST_ANNUL( EQFS, STATUS )
            END IF
         END IF
      END IF

*  Total intensity.
      CALL POL1_CNEWS( CI, 'I', .TRUE., CAT__TYPER, 0, UNITS, 'G13.6',
     :                'Total intensity', II, STATUS )

*  Total intensity standard deviation (if variances are available).
      IF( VAR ) THEN
         CALL POL1_CNEWS( CI, 'DI',  .TRUE., CAT__TYPER, 0, UNITS,
     :                   'G13.6', 'Standard deviation on total '//
     :                   'intensity', II, STATUS )
         CALL CAT_TATTL( II, 'PRFDSP', .FALSE., STATUS )
      END IF

*  Q and U (if not circular).
      IF( .NOT. CIRC ) THEN
         CALL POL1_CNEWS( CI, 'Q', .TRUE., CAT__TYPER, 0, UNITS,
     :                   'G13.6', 'Stokes parameter Q', II, STATUS )
         CALL CAT_TATTL( II, 'PRFDSP', .FALSE., STATUS )

         IF( VAR ) THEN
            CALL POL1_CNEWS( CI, 'DQ', .TRUE., CAT__TYPER, 0, UNITS,
     :                       'G13.6', 'Standard deviation on Q', II,
     :                       STATUS )
            CALL CAT_TATTL( II, 'PRFDSP', .FALSE., STATUS )
         END IF

         CALL POL1_CNEWS( CI, 'U', .TRUE., CAT__TYPER, 0, UNITS,
     :                   'G13.6', 'Stokes parameter U', II, STATUS )
         CALL CAT_TATTL( II, 'PRFDSP', .FALSE., STATUS )

         IF( VAR ) THEN
            CALL POL1_CNEWS( CI, 'DU', .TRUE., CAT__TYPER, 0, UNITS,
     :                       'G13.6', 'Standard deviation on U', II,
     :                       STATUS )
            CALL CAT_TATTL( II, 'PRFDSP', .FALSE., STATUS )
         END IF

*  V (if circular).
      ELSE
         CALL POL1_CNEWS( CI, 'V', .TRUE., CAT__TYPER, 0, UNITS,
     :                    'G13.6', 'Stokes parameter V', II, STATUS )
         CALL CAT_TATTL( II, 'PRFDSP', .FALSE., STATUS )

         IF( VAR ) THEN
            CALL POL1_CNEWS( CI, 'DV', .TRUE., CAT__TYPER, 0, UNITS,
     :                       'G13.6', 'Standard deviation on V', II,
     :                       STATUS )
            CALL CAT_TATTL( II, 'PRFDSP', .FALSE., STATUS )
         END IF
      END IF

*  Percentage polarisation.
      CALL POL1_CNEWS( CI, 'P', .FALSE., CAT__TYPER, 0, '%', 'F6.2',
     :                'Percentage polarisation', II, STATUS )

      IF( VAR ) THEN
         CALL POL1_CNEWS( CI, 'DP', (II .NE. CAT__NOID), CAT__TYPER, 0,
     :                    '%', 'F6.3', 'Standard deviation on '//
     :                    'percentage polarisation', II, STATUS )
         IF( II .NE. CAT__NOID ) CALL CAT_TATTL( II, 'PRFDSP', .FALSE.,
     :                                           STATUS )
      END IF

*  Polarisation angle (degrees).
      CALL POL1_CNEWS( CI, 'ANG', .FALSE., CAT__TYPER, 0, 'Degrees',
     :                'F6.2', 'Polarisation angle', II, STATUS )

      IF( VAR ) THEN
         CALL POL1_CNEWS( CI, 'DANG', (II .NE. CAT__NOID), CAT__TYPER,
     :                    0, 'Degrees', 'F6.3', 'Standard deviation '//
     :                    'on polarisation angle', II, STATUS )
         IF( II .NE. CAT__NOID ) CALL CAT_TATTL( II, 'PRFDSP', .FALSE.,
     :                                           STATUS )
      END IF

*  Polarised intensity.
      CALL POL1_CNEWS( CI, 'PI', .FALSE., CAT__TYPER, 0, UNITS, 'G13.6',
     :                'Polarised intensity', II, STATUS )

      IF( VAR ) THEN
         CALL POL1_CNEWS( CI, 'DPI', (II .NE. CAT__NOID), CAT__TYPER, 0,
     :                    UNITS, 'G13.6', 'Standard deviation on '//
     :                    'polarised intensity', II, STATUS )
         IF( II .NE. CAT__NOID ) CALL CAT_TATTL( II, 'PRFDSP', .FALSE.,
     :                                           STATUS )
      END IF

*  Add a Frame describing the reference direction to the supplied WCS
*  FrameSet.
      CALL POL1_PTANG( ANGROT, IWCS, STATUS )

*  Store the POLPACK version string as a catalogue parameter.
      CALL POL1_PTVRC( CI, STATUS )

*  Annul the pointer to the Base Fame.
      CALL AST_ANNUL( FRM, STATUS )

      END
