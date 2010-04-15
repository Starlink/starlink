      SUBROUTINE SPD_PBAA( T_VARXST, T_NDATA, T_XDAT, T_YBIN, T_YVAR,
     :   T_YCRS, T_YERL, T_YERU, STATUS )
*+
*  Name:
*     SPD_PBAA

*  Purpose:
*     Register spectrum and model.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_PBAA( VARXST, NDATA, XDAT, YBIN, YVAR,
*        YCRS, YERL, YERU, STATUS )

*  Description:
*     This routine registers the given arrays as a spectrum. This
*     includes intialisation of the controls for the interactive PGPLOT
*     graphics.
*
*     This routine only registers the arrays, it does not allocate the
*     arrays. The arrays must have been mapped by the calling routine.
*     In fact one or three work spaces must be provided in addition to
*     x, y and variance.
*
*     The y value arrays may contain bad values, these are taken care of
*     properly.

*  Arguments:
*     VARXST = LOGICAL
*        True if the spectrum has a variance array.
*     NDATA = INTEGER
*        The size of the arrays that XDAT, YBIN, YVAR, YCRS, YER{LU}
*        point to.
*     XDAT = INTEGER
*        The pointer to the x array, which is REAL and has length NDATA.
*     YBIN = INTEGER
*        The pointer to a REAL array of NDATA y values. YBIN(XDAT) is
*        plotted in the bottom viewport with bin style, ignoring bad
*        values.
*     YVAR = INTEGER
*        The pointer to a REAL array of NDATA variance values.
*        YVAR(XDAT) is used to plot error bars in the upper viewport.
*     YCRS = INTEGER
*        The pointer to a REAL work space for NDATA values. YCRS(XDAT)
*        is calculated prior to display by subtracting the model from
*        YBIN. It is plotted in the top viewport as crosses, ignoring
*        bad values.
*     YER{LU} = INTEGER
*        The pointers to two REAL work spaces of NDATA values.
*        YER{LU}(XDAT) are calculated prior to display as YCRS -+
*        SQRT(YVAR). They are plotted in the top viewport as y error
*        bars, ignoring bad values. These work spaces are needed only if
*        a variance is registered.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29 Apr 1994 (hme):
*        Original version.
*     2005 June 2 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Global Variables:
      INCLUDE 'SPD_PCOM'         ! Specdre SPLOOP common block

*  Arguments Given:
      LOGICAL T_VARXST
      INTEGER T_NDATA
      INTEGER T_XDAT, T_YBIN, T_YVAR
      INTEGER T_YCRS, T_YERL, T_YERU

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL DELTA                 ! Value range

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  De-register any existing image.
      SPXST = .FALSE.
      XDAT = 0
      YBIN = 0
      YVAR = 0
      YCRS = 0
      YERL = 0
      YERU = 0
      NDATA = 0

*  Store existence flags.
      VARXST = T_VARXST

*  Set the array sizes
      NDATA = T_NDATA

*  Store array pointers.
      XDAT = T_XDAT
      YBIN = T_YBIN
      YCRS = T_YCRS
      IF ( VARXST ) THEN
         YVAR = T_YVAR
         YERL = T_YERL
         YERU = T_YERU
      END IF

*  Work out bottom viewport windows.
      CALL SPD_PEAAR( .FALSE., NDATA, %VAL( CNF_PVAL(XDAT) ),
     :                SPWIN(1), SPWIN(2), STATUS )
      CALL SPD_PEAAR( .TRUE., NDATA, %VAL( CNF_PVAL(YBIN) ),
     :                SPWIN(3), SPWIN(4), STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      DELTA = SPWIN(2) - SPWIN(1)
      SPWIN(1) = SPWIN(1) - DELTA / 40.
      SPWIN(2) = SPWIN(2) + DELTA / 40.
      DELTA = SPWIN(4) - SPWIN(3)
      SPWIN(3) = SPWIN(3) - DELTA / 20.
      SPWIN(4) = SPWIN(4) + DELTA / 20.
      SPRNG(1) = SPWIN(1)
      SPRNG(2) = SPWIN(2)
      SPRNG(3) = SPWIN(3)
      SPRNG(4) = SPWIN(4)

*  Set flag.
      IF ( STATUS .EQ. SAI__OK ) SPXST = .TRUE.

*  Return.
 500  CONTINUE
      END
