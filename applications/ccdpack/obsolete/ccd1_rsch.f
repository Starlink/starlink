      SUBROUTINE CCD1_RSCH( GIDIN, FTYPES, NNDF, VALID, MKBIAS, MKDARK,
     :                      MKFLAS, MKFLAT, HVFLAT, FILNMS, NFILS,
     :                      PTEMP, STATUS )
*+
*  Name:
*     CCD1_RSCH

*  Purpose:
*     Reports the intended reduction schedule.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_RSCH( GIDIN, FTYPES, NNDF, VALID, MKBIAS,
*                     MKDARK, MKFLAS, MKFLAT, HVFLAT, FILNMS, NFILS,
*                     PTEMP, STATUS )

*  Description:
*     This routine writes out the intended reduction schedule to the
*     user.

*  Arguments:
*     GIDIN = INTEGER (Given)
*        IRG input group identifier.
*     FTYPES( 2, NNDF ) = CHARACTER * ( * ) (Given)
*        The frame and filter types of the NDFs in GID.
*     NNDF = INTEGER (Given)
*        Number of input NDFs input.
*     VALID( NNDF ) = LOGICAL (Given)
*        Array of flags indicating which NDFs are to be used.
*     MKBIAS = LOGICAL (Given)
*        Whether a MASTER_BIAS frame is to be produced.
*     MKDARK = LOGICAL (Given)
*        Whether a MASTER_DARK frames is to be produced.
*     MKFLAS = LOGICAL (Given)
*        Whether a MASTER_FLASH frame is to be produced.
*     MKFLAT( NNDF ) = LOGICAL (Given)
*        Whether a MASTER_FLAT for the corresponding FILTER type
*        (recorded in FILNMS) is required.
*     HVFLAT( NNDF ) = LOGICAL (Given)
*        Whether FLATs corresponding to the FILTER type (recorded in
*        FILNMS) are available.
*     FILNMS( NNDF ) = CHARACTER * ( * ) (Given)
*        The FILTER types of the flatfields. This is a unique list of
*        the filters specified in FTYPES.
*     NFILS = INTEGER (Given)
*        Number of entries in FILNMS (i.e. number of possible
*        flatfields).
*     PTEMP( NNDF ) = INTEGER (Given and Returned)
*        Workspace.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-FEB-1992 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER GIDIN
      INTEGER NNDF
      CHARACTER * ( * ) FTYPES( 2, NNDF )
      LOGICAL VALID( NNDF )
      LOGICAL MKBIAS
      LOGICAL MKDARK
      LOGICAL MKFLAS
      LOGICAL MKFLAT( NNDF )
      LOGICAL HVFLAT( NNDF )
      INTEGER NFILS
      CHARACTER * ( * ) FILNMS( NNDF )

*  Arguments Given and Returned
      INTEGER PTEMP( NNDF )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Loop variable
      INTEGER IPOINT             ! Dummy
      INTEGER NFRMS              ! Number of frames located
      INTEGER NMAST              ! Number of master_frames
      CHARACTER * ( 80 ) DEVICE  ! Device name
      CHARACTER * ( 80 ) NAME    ! NDF name
      CHARACTER * ( 80 ) DIRN    ! Directory
      CHARACTER * ( 80 ) AMODE   ! Access mode
      CHARACTER * ( 80 ) SLICE   ! NDF slice spec.
      LOGICAL OUT                ! Dummy
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report intentions to user.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '  Reduction Schedule', STATUS )
      CALL CCD1_MSG( ' ', '  ------------------', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )


*  Say if MASTER_BIAS is to be produced, and which one is to be used.
      IF ( MKBIAS ) THEN
         CALL CCD1_MSG( ' ', '  MASTER_BIAS frame will be produced'//
     :   ' using NDFs:', STATUS )

*  Get pointers to BIAS frames.
         CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, 'BIAS', PTEMP,
     :                    NFRMS, STATUS )
         DO 1 I = 1, NFRMS
            IPOINT = PTEMP( I )
            CALL IRG_GET( GIDIN, IPOINT, DEVICE, DIRN, NAME, SLICE,
     :                    AMODE, OUT, STATUS )
            CALL MSG_SETC( 'DEVICE', DEVICE )
            CALL MSG_SETC( 'NAME', NAME )
            CALL MSG_SETC( 'DIRN', DIRN )
            CALL CCD1_MSG( ' ', '  ^DEVICE^DIRN^NAME' , STATUS )
 1       CONTINUE
      ELSE

*  Do we have a MASTER_BIAS already ?
         CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, 'MASTER_BIAS',
     :                    PTEMP, NMAST, STATUS )
         IF ( NMAST .GT. 0 ) THEN

*  Yes we do, report its name.
            IPOINT = PTEMP( 1 )
            CALL IRG_GET( GIDIN, IPOINT, DEVICE, DIRN, NAME, SLICE,
     :                    AMODE, OUT, STATUS )
            CALL MSG_SETC( 'DEVICE', DEVICE )
            CALL MSG_SETC( 'NAME', NAME )
            CALL MSG_SETC( 'DIRN', DIRN )
            CALL CCD1_MSG( ' ', '  Using MASTER_BIAS :'//
     :      ' ^DEVICE^DIRN^NAME', STATUS )
         ELSE

*  No MASTER_BIAS and will not produce one. Report this.
            CALL CCD1_MSG( ' ', '  Debiassing will be performed '//
     :      'using interpolation or by subtraction of a constant',
     :      STATUS )
         END IF
      END IF

*  Which frames will be debiassed?
      CALL CCD1_MSG( ' ', '  The following NDFs will be debiassed:',
     :               STATUS )

*  Targets.
      CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, 'TARGET', PTEMP,
     :                 NFRMS, STATUS )
      IF ( NFRMS .GT. 0 ) THEN
         DO 2 I = 1, NFRMS
            IPOINT = PTEMP( I )
            CALL IRG_GET( GIDIN, IPOINT, DEVICE, DIRN, NAME, SLICE,
     :                    AMODE, OUT, STATUS )
            CALL MSG_SETC( 'DEVICE', DEVICE )
            CALL MSG_SETC( 'NAME', NAME )
            CALL MSG_SETC( 'DIRN', DIRN )
            CALL CCD1_MSG( ' ', '  ^DEVICE^DIRN^NAME' , STATUS )
 2       CONTINUE
      END IF

*  FLATs.
      CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, 'FLAT', PTEMP,
     :                 NFRMS, STATUS )
      IF ( NFRMS .GT. 0 ) THEN
         DO 3 I = 1, NFRMS
            IPOINT = PTEMP( I )
            CALL IRG_GET( GIDIN, IPOINT, DEVICE, DIRN, NAME, SLICE,
     :                    AMODE, OUT, STATUS )
            CALL MSG_SETC( 'DEVICE', DEVICE )
            CALL MSG_SETC( 'NAME', NAME )
            CALL MSG_SETC( 'DIRN', DIRN )
            CALL CCD1_MSG( ' ', '  ^DEVICE^DIRN^NAME' , STATUS )
 3       CONTINUE
      END IF

*  DARKs.
      CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, 'DARK', PTEMP,
     :                 NFRMS, STATUS )
      IF ( NFRMS .GT. 0 ) THEN
         DO 4 I = 1, NFRMS
            IPOINT = PTEMP( I )
            CALL IRG_GET( GIDIN, IPOINT, DEVICE, DIRN, NAME, SLICE,
     :                    AMODE, OUT, STATUS )
            CALL MSG_SETC( 'DEVICE', DEVICE )
            CALL MSG_SETC( 'NAME', NAME )
            CALL MSG_SETC( 'DIRN', DIRN )
            CALL CCD1_MSG( ' ', '  ^DEVICE^DIRN^NAME' , STATUS )
 4       CONTINUE
      END IF

*  FLASHes.
      CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, 'FLASH', PTEMP,
     :                 NFRMS, STATUS )
      IF ( NFRMS .GT. 0 ) THEN
         DO 5 I = 1, NFRMS
            IPOINT = PTEMP( I )
            CALL IRG_GET( GIDIN, IPOINT, DEVICE, DIRN, NAME, SLICE,
     :                    AMODE, OUT, STATUS )
            CALL MSG_SETC( 'DEVICE', DEVICE )
            CALL MSG_SETC( 'NAME', NAME )
            CALL MSG_SETC( 'DIRN', DIRN )
            CALL CCD1_MSG( ' ', '  ^DEVICE^DIRN^NAME' , STATUS )
 5       CONTINUE
      END IF


*  Are we going to process or use a MASTER_DARK?
      IF ( MKDARK ) THEN
         CALL CCD1_MSG( ' ', '  MASTER_DARK frame will be produced'//
     :   ' using NDFs:', STATUS )

*  Going to make one. Get pointers to DARK frames.
         CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, 'DARK', PTEMP,
     :                    NFRMS, STATUS )
         DO 6 I = 1, NFRMS
            IPOINT = PTEMP( I )
            CALL IRG_GET( GIDIN, IPOINT, DEVICE, DIRN, NAME, SLICE,
     :                    AMODE, OUT, STATUS )
            CALL MSG_SETC( 'DEVICE', DEVICE )
            CALL MSG_SETC( 'NAME', NAME )
            CALL MSG_SETC( 'DIRN', DIRN )
            CALL CCD1_MSG( ' ', '  ^DEVICE^DIRN^NAME' , STATUS )
 6       CONTINUE
      ELSE

*  Have we got one already?
         CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, 'MASTER_DARK',
     :                    PTEMP, NMAST, STATUS )
         IF ( NMAST .GT. 0 ) THEN

*  Going to use MASTER_DARK that exists already.
            IPOINT = PTEMP( 1 )
            CALL IRG_GET( GIDIN, IPOINT, DEVICE, DIRN, NAME, SLICE,
     :                    AMODE, OUT, STATUS )
            CALL MSG_SETC( 'DEVICE', DEVICE )
            CALL MSG_SETC( 'NAME', NAME )
            CALL MSG_SETC( 'DIRN', DIRN )
            CALL CCD1_MSG( ' ', '  Using MASTER_DARK :'//
     :      ' ^DEVICE^DIRN^NAME', STATUS )
         END IF
      END IF

*  Are we going to process or use a MASTER_FLASH?
      IF ( MKFLAS ) THEN
         CALL CCD1_MSG( ' ', '  MASTER_FLASH frame will be produced'//
     :   ' using NDFs:', STATUS )

*  Going to make one. Get pointers to DARK frames.
         CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, 'FLASH', PTEMP,
     :                    NFRMS, STATUS )
         DO 7 I = 1, NFRMS
            IPOINT = PTEMP( I )
            CALL IRG_GET( GIDIN, IPOINT, DEVICE, DIRN, NAME, SLICE,
     :                    AMODE, OUT, STATUS )
            CALL MSG_SETC( 'DEVICE', DEVICE )
            CALL MSG_SETC( 'NAME', NAME )
            CALL MSG_SETC( 'DIRN', DIRN )
            CALL CCD1_MSG( ' ', '  ^DEVICE^DIRN^NAME' , STATUS )
 7       CONTINUE
      ELSE

*  Have we got one already?
         CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, 'MASTER_FLASH',
     :                    PTEMP, NMAST, STATUS )
         IF ( NMAST .GT. 0 ) THEN

*  Going to use MASTER_DARK that exists already.
            IPOINT = PTEMP( 1 )
            CALL IRG_GET( GIDIN, IPOINT, DEVICE, DIRN, NAME, SLICE,
     :                    AMODE, OUT, STATUS )
            CALL MSG_SETC( 'DEVICE', DEVICE )
            CALL MSG_SETC( 'NAME', NAME )
            CALL MSG_SETC( 'DIRN', DIRN )
            CALL CCD1_MSG( ' ', '  Using MASTER_FLASH :'//
     :      ' ^DEVICE^DIRN^NAME', STATUS )
         END IF
      END IF

*  What's going to happen on the flatfield front? Explain for each
*  filter type.
      DO 8 J = 1, NFILS
         IF ( HVFLAT( J ) ) THEN

*  Will be using a MASTER_FLAT for this filter, get a pointer to it.
            CALL CCD1_LOCS3( FTYPES, 2, NNDF, 1, 2, VALID,
     :                       'MASTER_FLAT', FILNMS( J ), PTEMP, NMAST,
     :                       STATUS )

*  Going to use MASTER_DARK that exists already.
            IPOINT = PTEMP( 1 )
            CALL IRG_GET( GIDIN, IPOINT, DEVICE, DIRN, NAME, SLICE,
     :                    AMODE, OUT, STATUS )
            CALL MSG_SETC( 'DEVICE', DEVICE )
            CALL MSG_SETC( 'NAME', NAME )
            CALL MSG_SETC( 'DIRN', DIRN )
            CALL MSG_SETC( 'FILTER', FILNMS( J ) )
            CALL CCD1_MSG( ' ', '  Using MASTER_FLAT:'//
     :      ' ^DEVICE^DIRN^NAME, for FILTER ^FILTER', STATUS )
        ELSE IF ( .NOT. HVFLAT( J ) .AND. MKFLAT( J ) ) THEN

*  Will be making a flatfield for this filter.
            CALL MSG_SETC( 'FILTER', FILNMS( J ) )
            CALL CCD1_MSG( ' ',
     :'  MASTER_FLAT frame for filter ^FILTER will be produced'//
     :      ' using NDFs:', STATUS )

*  Going to make one. Get pointers to DARK frames.
            CALL CCD1_LOCS3( FTYPES, 2, NNDF, 1, 2, VALID, 'FLAT',
     :                       FILNMS( J ), PTEMP, NFRMS, STATUS )
            DO 9 I = 1, NFRMS
               IPOINT = PTEMP( I )
               CALL IRG_GET( GIDIN, IPOINT, DEVICE, DIRN, NAME, SLICE,
     :                       AMODE, OUT, STATUS )
               CALL MSG_SETC( 'DEVICE', DEVICE )
               CALL MSG_SETC( 'NAME', NAME )
               CALL MSG_SETC( 'DIRN', DIRN )
               CALL CCD1_MSG( ' ', '  ^DEVICE^DIRN^NAME' , STATUS )
 9          CONTINUE
         ELSE

*  No flatfielding possible for this FILTER.
            CALL MSG_SETC( 'FILTER', FILNMS( J ) )
            CALL CCD1_MSG( ' ', '  Flatfielding will not be performed'//
     :      'on data with a filter type of ^FILTER', STATUS )
         END IF
 8    CONTINUE

      END
* $Id$
