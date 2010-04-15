*+FORM_PAGE_LIMITS Gets Useful field numbers etc.
*  Notes
*     On the DSCF file:
*     1) the no. of targets must be the last entry on the Cover page
*     2) the target number  ::		first entry on the Target page
*	DATE		AUTHOR			DESCRIPTION
*	???		RAL			ORIGINAL
*	9 APR 1992	M. DUESTERHAUS (GSFC)	PORT TO UNIX
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*     1994 Jan		M Ricketts	RAL version
**************************************************************************
      SUBROUTINE FORM_PAGE_LIMITS(STATUS)
      IMPLICIT NONE

*  Calling Argument
      INTEGER STATUS			! Out	0=OK, -1 = bad dscf file

*  Global Variables
      INCLUDE 'com_form_points.inc'	! Common to pass data
      INCLUDE 'com_form_files.inc'

*  Functions
      INTEGER DBS_FIELDNO, DBS_INFOI


*  Executable Code

      FLD_NTARGETS = DBS_FIELDNO( REF_FORM , 'NUMBER.OF.TARGETS' )
      IF (FLD_NTARGETS .LE.0) GOTO 90
      FLD_TARG_NUMBER = DBS_FIELDNO( REF_TARGET , 'TARGET.NUMBER' )
      IF (FLD_TARG_NUMBER .LE.0) GOTO 90
      FLD_CONSTRAINTS = DBS_FIELDNO( REF_TARGET , 'TIME.CRITICAL' )
      IF (FLD_CONSTRAINTS .LE.0) GOTO 90

      FLD_LIMS_COVER(1) = 1							! Cover page
      FLD_LIMS_COVER(2) = FLD_NTARGETS - 1					! 	Omit No. of targets
      FLD_LIMS_GEN(1) = FLD_NTARGETS + 1					! General page
      FLD_LIMS_GEN(2) = DBS_INFOI( REF_FORM , 1 , 'NFIELDS' )

      FLD_LIMS_TARG(1) = FLD_TARG_NUMBER					! Target page
      FLD_LIMS_CONS(1) = DBS_FIELDNO( REF_TARGET , 'COORD.OBSERVATION' )	! Constraints page
      FLD_LIMS_TARG(2) = FLD_LIMS_CONS(1) - 1
      FLD_LIMS_CONS(2) = DBS_INFOI( REF_TARGET , 1 , 'NFIELDS' )

      STATUS = 0
      GOTO 99

90    CONTINUE
      CALL FORM_ERR('DSCF File Contents Error')
      STATUS = -1
99    CONTINUE

      END
