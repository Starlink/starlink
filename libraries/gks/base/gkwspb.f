C# IL>=a, OL>=2
      SUBROUTINE GKWSPB(IXWKID,ISGNAM,TRNC2,SEGT3,XCLSUB,LHCLIP)
*---------------------------------------------------------------------
*
* (C) COPYRIGHT ICL & SERC  1985
*
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Front End
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*      WISS - Playback Segment
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/85  MGC   Original version stabilized.
*     20/01/87  ARG   IS conversion. Error number changed.
*
*  ARGUMENTS
*  ---------
*     INP  IXWKID   Workstation Identifier or nil (see comment)
*     INP  ISGNAM   Segment name
*     INP  TRNC2    Transformation C2 - combined insert transformation
*     INP  SEGT3    Transformation t3 - segment transformation
*     INP  XCLSUB   Interface to workstation(s) supplied by Front End
*     INP  LHCLIP   Clipping rectangles honoured/ignored
*
      INTEGER IXWKID,ISGNAM
      REAL TRNC2(6),SEGT3(6)
      LOGICAL LHCLIP
      EXTERNAL XCLSUB
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /ERR/    KERROR passed on to caller. Modified when the
*                     saved value (in IERR) is restored. Effect though
*                     is to preserve the value of the first error to
*                     be detected.
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     ISTAT   Status following select segment in WISS
*     IERR    Local copy of KERROR
*     TRNC3   Transformation C3
*
      INTEGER ISTAT,IERR
      REAL    TRNC3(6)
*
*  EXTERNALS
*  ---------
*     GK0WRD : WISS segment read routine
*
      EXTERNAL GK0WRD
*
*  ERRORS
*  ------
*     -2020  Segment does not exist in WISS
*
*  COMMENTS
*  --------
*  The workstation identifier supplied to this routine is not used
*  other than to be passed down to the segment playback routine.
*  The identifier may be a valid workstation identifier, or it may
*  be nil.
*
*---------------------------------------------------------------------

*     transformationC3 <- t3 x C2
      CALL GKMTML(TRNC2,SEGT3,TRNC3)

*     select segment in wiss
      CALL GK0WSL(ISGNAM,ISTAT)

      IF(ISTAT.EQ.KRFUSE) THEN
        CALL GKBUG (-2020,'GKSGPB')
      ELSE
*       playback segment from wiss to workstation(s)
        CALL GKSGCN(IXWKID,TRNC2,TRNC3,GK0WRD,XCLSUB,LHCLIP)
*       release segment in wiss if error
        IF(KERROR.NE.0) THEN
          IERR=KERROR
          CALL GK0WRL
          KERROR=IERR
        ENDIF
      ENDIF

      RETURN
      END
