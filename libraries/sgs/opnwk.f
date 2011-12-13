      SUBROUTINE sgs_OPNWK (WKSTN, IZONID, JSTAT)
*+
*  Name:
*     OPNWK

*  Purpose:
*     Open a workstation and set text representations (unless the
*     workstation is already open).  Create a base zone and select it.

*  Language:
*     Starlink Fortran 77

*  Description:
*     The base zone fills the display surface, with a window that
*     extends from (0,0) to (x,y), where the x/y is the true aspect
*     ratio of the display surface and the smaller of x or y is
*     unity.
*
*     This routine contains a workaround for a bug in the RAL GKS.

*  Arguments:
*     WKSTN = CHAR (Given)
*         Workstation name (a character string)
*     IZONID = INTEGER (Returned)
*         Zone identifier
*     JSTAT = INTEGER (Given & Returned)
*         Inherited status (if option selected)
*         Status: 0=OK (if non-inherited)

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     07-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Constants From GKS_PAR:
*     GOUTPT     i      workstation category - Output
*     GINPUT     i           "          "    - Input
*     GINOUT     i           "          "    - Input/output
*     GMETRE     i      device units - Metres
*     GBNIG      i      deferral state - Before next interaction globally
*     GSUPPD     i      regeneration mode - suppressed
*     GYES       I      Yes

*  Constants From Sgscom:
*     MXWK       i      maximum number of workstations allowed
*     MXZ        i      maximum number of zones allowed

*  Errors:
*     Bad workstation name
*     Bad workstation type
*     Too many workstations
*     Too many zones
*     Workstation could not be opened
*     Error returned by GKS inquiry

*  Externals:
*     sgs_1HSTAT, sgs_FLUSH, sgs_WIDEN, sgs_1ERR, sgs_1GKERR,
*     sgs_1NORM, sgs_1WDTRD, sgs_1GETZ, sgs_1NEWZ, sgs_1SETTX,
*     GQWKCA, GQWKC, GOPWK, GQDSP, GSWKVP, GSWKVP,
*     GSWKWN, GSDS, GSTXFP, GQWKM, GPREC, GESC

*  Read From Common:
*     IWTID      i()    workstation table - GKS workstation ID
*     IWTTY      i()    workstation table - type
*     IWTCO      i()    workstation table - connection ID
*     IWTCA      i()    workstation table - category
*     IZTW       i()    zone table - SGS workstation ID
*     NSCLOP     l()    WDT - no screen clear supported
*     IFONT      i      current font
*     IPREC      i          "    text precision
*     NCLORQ     l      no clear open requested

*  Written To Common:
*     NCLORQ     l       no clear open requested
*     WSNRCL     l       workstation not really clear

*-

      IMPLICIT NONE

      CHARACTER*(*) WKSTN
      INTEGER IZONID,JSTAT

      INCLUDE 'sgscom'

      INCLUDE 'GKS_PAR'

      INCLUDE 'SGS_ERR'


      CHARACTER RNAME*5, RESET*7
      PARAMETER (RNAME='OPNWK')
      REAL XM,YM,XN,YN
      INTEGER IWKID,IERR,IUNIT,ICAT,IGWKID,ISWKID
      INTEGER DUMMY1,DUMMY2,N,IWKTY,ICON,J
      INTEGER MXOPWK,MXACWK,MXWKAS

*  Declarations for Starlink "no clear on open" escape function
      INTEGER IA(2), IER, LIESC, LOESC
      CHARACTER*80 STR(1), IESCDR(1), OESCDR(1)
      REAL RA(1)
      DATA STR/' '/



*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Complete outstanding plotting
      CALL sgs_FLUSH

*  Translate the workstation name into the GKS workstation type and
*  connection identifier
      CALL sgs_WIDEN(WKSTN,IWKTY,ICON,J)
      IF (J.NE.0) THEN
         CALL sgs_1ERR(SGS__BADWK,RNAME,'Bad workstation name',JSTAT)
         GO TO 9999
      END IF

*   Check that it's a valid type
      CALL GQWKCA(IWKTY,IERR,ICAT)
      IF (IERR.NE.0) THEN
         CALL sgs_1ERR(SGS__BADWT,RNAME,'Bad workstation type',JSTAT)
         GO TO 9999
      END IF

*  Allocate an SGS workstation ID
      DO 30 ISWKID = 1,MXWK
         IF (IWTID(ISWKID).EQ.0) GO TO 40
   30 CONTINUE
      CALL sgs_1ERR(SGS__WRKEX,RNAME,'Too many workstations',JSTAT)
      GO TO 9999
   40 CONTINUE

*  Is this GKS workstation already open?
      IWKID=0
      N=0
   10 CONTINUE
      IF (N.GE.MXWK.OR.IWKID.NE.0) GO TO 20
         N=N+1
         IF (IWTTY(N).EQ.IWKTY
     :  .AND.IWTCO(N).EQ.ICON) IWKID=N
      GO TO 10
   20 CONTINUE

      IF (IWKID.EQ.0) THEN

*     New workstation

*     Allocate a GKS workstation ID
         CALL GQWKM(IERR,MXOPWK,MXACWK,MXWKAS)
         IF (IERR.NE.0) THEN
            CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQWKM',
     :                                                            JSTAT)
         GO TO 9999
         END IF
         DO 50 IGWKID = 1,MXOPWK
            CALL GQWKC(IGWKID,IERR,DUMMY1,DUMMY2)
            IF (IERR.NE.0) GO TO 100
   50    CONTINUE

  100    CONTINUE

*     If the "no clear open requested" flag has been set, then call GESC
*     to set the state of the GKS device handler
         IF (NCLORQ) THEN

*     Check that this workstation supports it
            CALL gns_ITWCG( IWKTY, "OPEN", RESET, JSTAT)
            IF (JSTAT.NE.0) GO TO 9999
            IF (RESET.EQ."NORESET") THEN

               IA(1) = IWKTY
               IA(2) = GYES
               CALL GPREC(2, IA, 1, RA, 1, 1, STR, 1, IER, LIESC,
     :                    IESCDR)
               CALL GESC(-3, LIESC, IESCDR, 1, LOESC, OESCDR)
            END IF

*     Set workstation not really clear flag
            WSNRCL(ISWKID) = .TRUE.
         ELSE
            WSNRCL(ISWKID) = .FALSE.
         END IF

*     Open the workstation
         CALL GOPWK(IGWKID,ICON,IWKTY)

*     Check workstation now open
         CALL sgs_1GKERR(RNAME,JSTAT)
         IF (JSTAT.NE.0) GO TO 9999

*     Get maximum display surface
         IF ( ICAT.EQ.GOUTPT .OR. ICAT.EQ.GINPUT .OR.
     :        ICAT.EQ.GOUTIN ) THEN
            CALL GQDSP(IWKTY,IERR,IUNIT,XM,YM,DUMMY1,DUMMY2)
            IF (IERR.NE.0) THEN
               CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQDSP',
     :                                                            JSTAT)
               GO TO 9999
            END IF

*        Set workstation viewport
            IF (IUNIT.EQ.GMETRE) THEN
               CALL GSWKVP(IGWKID,0.0,XM,0.0,YM)
            ELSE
*           Workaround of bug in RAL GKS
               XM = XM - 1
               YM = YM - 1
               CALL GSWKVP(IGWKID,0.0,XM,0.0,YM)
            END IF

*        Deduce dimensions of rectangle of same shape with one unit side
            CALL sgs_1NORM(XM,YM,XN,YN)

*        Set workstation window
            CALL GSWKWN(IGWKID,0.0,XN,0.0,YN)

*        Set deferral to maximum buffering
            CALL GSDS(IGWKID,GBNIG,GSUPPD)
         ELSE

*        Workstation is a metafile or segment storage so assume a square
*        display surface
            XN = 1.0
            YN = 1.0
         END IF

*     Make entry in SGS workstation table
         IWTID(ISWKID)=IGWKID
         IWTTY(ISWKID)=IWKTY
         IWTCO(ISWKID)=ICON
         IWTCA(ISWKID)=ICAT

*     Load SGS WDT for this workstation
         CALL sgs_1WDTRD(ISWKID,JSTAT)

      ELSE

*     Workstation already open

*     Make entry in SGS workstation table
         IWTID(ISWKID) = IWTID(IWKID)
         IWTTY(ISWKID) = IWTTY(IWKID)
         IWTCO(ISWKID) = IWTCO(IWKID)
         IWTCA(ISWKID) = IWTCA(IWKID)
         WSNRCL(ISWKID) = WSNRCL(IWKID)

*     Load SGS WDT for this workstation
         CALL sgs_1WDTRD(ISWKID,JSTAT)

*     Find the base zone for the existing workstation
         DO 120 J = 1, MXZ
            IF (IZTW(J).EQ.-IWKID) GO TO 140
  120    CONTINUE
         CALL sgs_1ERR(SGS__ZONNF,RNAME,'Specified zone does not exist',
     :                                                            JSTAT)
         GO TO 9999
  140    CONTINUE

*     Copy the base zone
         XN=ZTV(2,J)
         YN=ZTV(4,J)
      END IF

*  Allocate a zone table entry for a base zone
      CALL sgs_1GETZ(-ISWKID,IZONID)
      IF (IZONID.EQ.0) THEN
         CALL sgs_1ERR(SGS__ZNZEX,RNAME,'Too many zones',JSTAT)
         GO TO 9999
      END IF

*  Create and select the zone
      CALL sgs_1NEWZ(IZONID,0.0,XN,0.0,YN)

*  Set current SGS font and precision
      CALL GSTXFP(IFONT,IPREC)

*  Set text attributes
      CALL sgs_1SETTX

*  Reset no-screen-clear flag
      NCLORQ = .FALSE.

*  Check for GKS errors
      CALL sgs_1GKERR(RNAME,JSTAT)

*  Exit
9999  CONTINUE

      END
