      SUBROUTINE IRM_STPEN( NCURV, CURPEN, SOLID, INLPEN, AXSPEN, 
     :                      TCKPEN, NLBPEN, ALBPEN, TITPEN, STATUS )
*+
*  Name:
*     IRM_STPEN

*  Purpose:
*     Set pen number for various portions of an NCAR display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_STPEN( NCURV, CURPEN, SOLID, INLPEN, AXSPEN, 
*                     TCKPEN, NLBPEN, ALBPEN, TITPEN, STATUS )
*  Description:
*     The routine is used, together with a user version of AGCHCU,
*     AGCHAX and AGCHIL to set the pen number for each curve, axis 
*     lines, tick marks, numeric labels, axis labels and title of 
*     an NCAR display. Since AGCHCU, AGCHAX and AGCHIL can not be 
*     called by user program directly, the user program has to call
*     this routine to set the pen number before NCAR's drawing. The 
*     routine will save the pen number setting in a common block 
*     which will be accessed by the new AGCHCU, AGCHAX and AGCHIL 
*     to set the pen number when each portion is about to be drawn.
*
*     The title refered to is the title witten by NCAR (AUTOGRAPH)
*     routines EZY, EZMY etc. which have the name of 'T' and line
*     number of 100. The axis labels have the name either 'T' or
*     'B' or 'R' or 'L'.
*
*     To make this routine take effect, the user program must be
*     linked with the new version of AGCHCU, AGCHAX and AGCHIL 
*     explicitly instead of the default ones in NCAR library.
*
*  Prior Requirements:
*     Before calling this routine, a SGS graphic device must have been
*     opened.
*
*     The following settings of the GKS ASF flag are assumed (they are 
*     SGS initial settings ):
*         POLYLINE - bundled
*         TEXT - individual
    
*    
*  Arguments:
*     NCURV = INTEGER (Given)
*        The number of curves
*     CURPEN( NCURV ) = INTEGER (Given)
*        The SGS pen number for each curve.
*     SOLID = LOGICAL (Given)
*        The flag to show whether to draw the curves in the solid line
*        regardless of the pen number of the curves.  If it is true the
*        curves in the plot will all be solid regardless of their pen
*        number and graphic device, otherwise, the line types of the 
*        the curves depends on both their pen number and the graphic
*        device type.
*     INLPEN( NCURV ) = INTEGER (Given)
*        The SGS pen number for each in-line label.
*     AXSPEN = INTEGER (Given)
*        The pen number for the axis lines.
*     TCKPEN = INTEGER (Given)
*        The pen number for the tick marks.
*     NLBPEN = INTEGER (Given)
*        The pen number for the numeric labels.
*     ALBPEN = INTEGER (Given)
*        The pen number for the axis labels.
*     TITPEN = INTEGER (Given)
*        The pen number for the title of the display.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-FEB-1991 (WG):
*        Original version.
*     27-APR-1998 (DSB)
*        Corrected use of Starlink STATUS within call to GQPLR (where GKS 
*        LSTAT should have been used).
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Globel Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GKS_PAR'          ! GKS constants

*  Globel Variables:
      INCLUDE 'IRM_COM'           ! NCAR pen setting variable
*        MCM_OLDPX = INTEGER (Write)
*           The GKS polyline index of the original setting before
*           calling this routine.
*        MCM_OLDTX = INTEGER (Write)
*           The GKS text colour index of the original setting 
*           before calling this routine.
*        MCM_SOCUR = LOGICAL (Write)
*           The flag to show whether to draw curve in solid line.
*        MCM_NCURV = INTEGER (Write)
*           The number of curves which have had their pen number
*           set.
*        MCM_CRPN( MCM__MXCUR ) = INTEGER (Write)
*           Pen number for each curve.
*        MCM_INCL( MCM__MXCUR ) = INTEGER (Write)
*           Colour index for in-line labels.
*        MCM_AXPN = INTEGER (Write)
*           Pen number for axis lines.
*        MCM_TKPN = INTEGER (Write)
*           Pen number for tick marks.
*        MCM_NLBCL = INTEGER (Write)
*           Colour index for numeric labels.
*        MCM_ALBCL = INTEGER (Write)
*           Colour index for axis labels.
*        MCM_TITCL = INTEGER (Write)
*           Colour index for the title.
*        MCM_STDAT = LOGICAL (Write)
*           Data setting flag. 
      
*  Arguments Given:
      INTEGER NCURV
      INTEGER CURPEN( NCURV )
      LOGICAL SOLID
      INTEGER INLPEN( NCURV )
      INTEGER AXSPEN
      INTEGER TCKPEN
      INTEGER NLBPEN
      INTEGER ALBPEN
      INTEGER TITPEN

*  Status:
      INTEGER STATUS             ! Globel status

*  Local Variables:
      INTEGER LINCOL             ! Colour index of a linr
      INTEGER I                  ! Do loop index
      INTEGER IWKID              ! Current workstation ID
      INTEGER LNTYPE             ! Line type
      INTEGER LSTAT              ! Local status
      REAL LWIDTH                ! Line width

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the local status variable.
      LSTAT = 0

*  Get current workstation ID.
      CALL SGS_ICURW( IWKID )

*  Inquire original GKS polyline index and original GKS text 
*  colour index.
      CALL GQPLI( LSTAT, MCM_OLDPX )
      CALL GQTXCI( LSTAT, MCM_OLDTX )

*  Set flag to show whether to draw the curves in solid line.
      MCM_SOCUR = SOLID
 
*  Write the pen number for curves.
      DO I = 1, NCURV

*  Check whether the specified SGS curve pen exits.
         CALL GQPLR( IWKID, CURPEN( I ), GSET, LSTAT, LNTYPE, 
     :               LWIDTH, LINCOL )

*  If the specified SGS pen does not exists, exit.
         CALL GKS_GSTAT( STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  If the pen number exits, write it to the common block.
         MCM_CRPN( I ) = CURPEN( I )

*  Get the colour index for the pen of in-line label.
         CALL GQPLR( IWKID, INLPEN( I ), GSET, LSTAT, LNTYPE, LWIDTH,
     :               LINCOL )

*  Check error, if so, exit.
         CALL GKS_GSTAT( STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  If the pen number exits, write it to the common block.
         MCM_INCL( I ) = INLPEN( I )
      END DO

*  Write the number of curves which have been set pen number. 
      MCM_NCURV = NCURV

*  Get the colour index for the pen of numeric labels.
      CALL GQPLR( IWKID, NLBPEN, GSET, LSTAT, LNTYPE, LWIDTH, 
     :            MCM_NLBCL )
         
*  Check error, if so, exit.
      CALL GKS_GSTAT( STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Get the colour index for the pen of the title.
      CALL GQPLR( IWKID, TITPEN, GSET, LSTAT, LNTYPE, LWIDTH,
     :            MCM_TITCL )

*  Check error, if so, exit.
      CALL GKS_GSTAT( STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Get the colour index for the pen of axis labels.
      CALL GQPLR( IWKID, ALBPEN, GSET, LSTAT, LNTYPE, LWIDTH,
     :            MCM_ALBCL )

*  Check error, if so, exit.
      CALL GKS_GSTAT( STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Check the existance of the pen number for axis lines.
      CALL GQPLR( IWKID, AXSPEN, GSET, LSTAT, LNTYPE, LWIDTH,
     :            LINCOL )

*  If the pen number does not exist, exit.
      CALL GKS_GSTAT( STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  If the pen exists, write it to common block.
      MCM_AXPN = LINCOL

*  Check the existance of the pen number for tick marks.
      CALL GQPLR( IWKID, TCKPEN, GSET, LSTAT, LNTYPE, LWIDTH,
     :            LINCOL )

*  If the pen number does not exist, exit.
      CALL GKS_GSTAT( STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  If the pen number exits, write it to common block.
      MCM_TKPN = LINCOL

*  Set setting data flag to true
      MCM_STDAT = .TRUE.

 999  CONTINUE

      END
