      SUBROUTINE GRF_RSGRAF( STATUS )
*+
*  Name:
*     SUBROUTINE GRF_RSGRAF

*  Purpose:
*     Reset graph parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRF_RSGRAF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     This sets the contents of CMGRAF, CMPLOT and CMLINR to their
*     default parameter values (except Reset itself).

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       AT4 version.
*     06-JAN-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*       Conversion to GKS 7.2 graphics.
*     08-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Some restructuring and final conversion to SGP/16 style.
*     25-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Defnitions:
      IMPLICIT NONE

*  Local Constants:
      INTEGER MAXLABEL   ! Maximum length of label string.
      INTEGER MAXNAME    ! Maximum length of name string.
      INTEGER MAXTITLE   ! Maximum length of title string.
      PARAMETER ( MAXLABEL = 40, MAXTITLE = 80, MAXNAME = 16 )

*  Status:
      INTEGER STATUS     ! Global status.

*  Global Variables:
      INCLUDE 'CMGRAF'
      INCLUDE 'CMCOLR'
      INCLUDE 'CMLINR'

*  Local Variables:
      INTEGER I        ! Loop index.

      INTEGER GSOLI    ! GKS solid line (GSLN).
      INTEGER GLDASH   ! GKS dashed line (GSLN).
      INTEGER GLDOT    ! GKS dotted line (GSLN).
      INTEGER GLDASD   ! GKS dot-dashed line (GSLN).

*  Data:
      DATA GSOLI, GLDASH, GLDASD, GLDOT / 1, 2, 3, 4 /
*.

*   For CMGRAF:
      ZCLEAR = .TRUE.
      ERASED = .FALSE.
      DRAWN = .FALSE.
      IDRAWN = .FALSE.
      NOXL = .TRUE.
      NOYL = .TRUE.

      CALL STR_TERM( 0, MAXLABEL, XLAB )
      CALL STR_TERM( 0, MAXLABEL, XUN )
      CALL STR_TERM( 0, MAXLABEL, YLAB )
      CALL STR_TERM( 0, MAXLABEL, YUN )
      CALL STR_TERM( 0, MAXTITLE, GTITLE )

      NLEGND = 0
      HISTLN = .TRUE.
      XJST = .FALSE.
      YJST = .FALSE.
      XREV = 0
      YREV = 0
      XTIC = 0.0
      YTIC = 0.0
      XVRT( 1 ) = 0.0
      XVRT( 2 ) = 0.0
      YVRT( 1 ) = 0.0
      YVRT( 2 ) = 0.0

*   For CMLINR:
      DO I = 1, 4
         LINTYP( I ) = 1
         CALL STR_MOVE( 'SOLID\\', MAXNAME, LINSTY( 1, I ) )
      END DO

*   Load line types.
      LINTYP( 1 ) = GSOLI
      CALL STR_MOVE( 'SOLID\\', MAXNAME, LINSTY( 1, 1 ) )

      LINTYP( 2 ) = GLDASH
      CALL STR_MOVE( 'DASH\\', MAXNAME, LINSTY( 1, 2 ) )

      LINTYP( 3 ) = GLDOT
      CALL STR_MOVE( 'DOT\\', MAXNAME, LINSTY( 1, 3 ) )

      LINTYP( 4 ) = GLDASD
      CALL STR_MOVE( 'DOTDASH\\', MAXNAME, LINSTY( 1, 4 ) )

      LININD = 1

*   Call to GKS to reset linestyle.
      CALL GSLN( LINTYP( 1 ) )

*   For CMCOLR:
      TICUR = 1
      TIROT = .TRUE.
      NEWLUT = .TRUE.

      END
