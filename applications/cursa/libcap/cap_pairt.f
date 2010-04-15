      SUBROUTINE CAP_PAIRT (SECROW, NPAIR, PRMPR, SECPR, SEPN,
     :  PMULT, SMULT, PRTYP, MAXPR, SECWRK, OPAIR, OPRMPR, OSECPR,
     :  OSEPN, OPMULT, OSMULT, STATUS)
*+
*  Name:
*     CAP_PAIRT
*  Purpose:
*     Generate a list of objects for a given type of pairing.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PAIRT (SECROW, NPAIR, PRMPR, SECPR, SEPN, PMULT, SMULT,
*       PRTYP, MAXPR; SECWRK; OPAIR, OPRMPR, OSECPR, OSEPN, OPMULT,
*       OSMULT; STATUS)
*  Description:
*     Generate a list of objects for a given type of pairing.  The
*     routine is given a list corresponding to 'PRIMARY' pairing and
*     uses this to construct the required output list.
*  Arguments:
*     SECROW  =  INTEGER (Given)
*        Number of rows in the secondary catalogue.
*     NPAIR  =  INTEGER (Given)
*        Number of paired rows (note that the pairing corresponds to
*        'PRIMARY' pairing).
*     PRMPR(NPAIR)  =  INTEGER (Given)
*        List of pairs in the primary catalogue.
*     SECPR(NPAIR)  =  INTEGER (Given)
*        List of pairs in the secondary catalogue.
*     SEPN(NPAIR)  =  DOUBLE PRECISION (Given)
*        The separation between two paired objects (radians).
*     PMULT(NPAIR)  =  INTEGER (Given)
*        The number of primary objects which the row matches.
*     SMULT(NPAIR)  =  INTEGER (Given)
*        The number of secondary objects which the row matches.
*     PRTYP  =  CHARACTER*(*) (Given)
*        The type of pairing list that is is to generated.  The options
*        are as follows:
*        C  -  COMMON (common objects),
*        P  -  PRIMARY (all objects in the primary),
*        M  -  MOSAIC (common objects plus unpaired in both catalogues),
*        R  -  PRIMREJ (rejected objects in the primary),
*        A  -  ALLREJ (rejected objects in both catalogues).
*     MAXPR  =  INTEGER (Given)
*        Maximum number of permitted objects in the output pairing
*        list.
*     SECWRK(SECROW)  =  INTEGER (Work)
*        Work array; contains a list of secondary objects.
*     OPAIR  =  INTEGER (Returned)
*        Number of objects in the paired list.
*     OPRMPR(MAXPR)  =  INTEGER (Returned)
*        Output list of pairs in the primary catalogue.
*     OSECPR(MAXPR)  =  INTEGER (Returned)
*        Output list of pairs in the secondary catalogue.
*     OSEPN(MAXPR)  =  DOUBLE PRECISION (Returned)
*        Output separation between two paired objects (radians).
*     OPMULT(MAXPR)  =  INTEGER (Returned)
*        Output number of primary objects which the row matches.
*     OSMULT(MAXPR)  =  INTEGER (Returned)
*        Output number of secondary objects which the row matches.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If common pairing is required then
*       Extract the paired primary objects from the input list.
*     else if primary pairing is required then
*       Copy the input list.
*     else if mosaic pairing is required then
*       Copy the input list.
*       Determine the unpaired secondary objects.
*       Append the unpaired secondary objects to the output list.
*     else if primrej pairing is required then
*       Extract the unpaired primary objects from the input list.
*     else if allrej pairing is required then
*       Extract the unpaired primary objects from the input list.
*       Determine the unpaired secondary objects.
*       Append the unpaired secondary objects to the output list.
*     else
*       Set the status.
*       Report error: illegal type of pairing requested.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     27/4/94 (ACD): Original version.
*     18/8/99 (ACD): Added propogation of arrays containing the
*        separation of paired objects and the number of primary and
*        secondary matches for each paired row.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'       ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'       ! CAT symbolic constants.
*  Arguments Given:
      INTEGER
     :  SECROW,
     :  NPAIR,
     :  PRMPR(NPAIR),
     :  SECPR(NPAIR),
     :  PMULT(NPAIR),
     :  SMULT(NPAIR),
     :  MAXPR
      DOUBLE PRECISION
     :  SEPN(NPAIR)
      CHARACTER
     :  PRTYP*(*)
*  Arguments Given and Returned:
      INTEGER
     :  SECWRK(SECROW)
*  Arguments Returned:
      INTEGER
     :  OPAIR,
     :  OPRMPR(MAXPR),
     :  OSECPR(MAXPR),
     :  OPMULT(MAXPR),
     :  OSMULT(MAXPR)
      DOUBLE PRECISION
     :  OSEPN(MAXPR)
*  Status:
      INTEGER STATUS   ! Global status.
*  Local Constants:

*
*    The value stored for the separation or distance in the case of
*    rows which do not pair.  A nonesense value is used deliberately;
*    genuine separations cannot be negative.

      DOUBLE PRECISION UDIST
      PARAMETER (UDIST = -1.0D0)
*  Local Variables:
      INTEGER
     :  LOOP           ! Loop index.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Common pairing; extract the paired objects from the primary
*       list.

         IF (PRTYP .EQ. 'C') THEN
            OPAIR = 0

            DO LOOP = 1, NPAIR
               IF (SECPR(LOOP) .NE. CAT__UPAIR) THEN
                  OPAIR = OPAIR + 1

                  OPRMPR(OPAIR) = PRMPR(LOOP)
                  OSECPR(OPAIR) = SECPR(LOOP)
                  OSEPN(OPAIR)  = SEPN(LOOP)
                  OPMULT(OPAIR) = PMULT(LOOP)
                  OSMULT(OPAIR) = SMULT(LOOP)
               END IF
            END DO

*
*       Primary pairing; the input list corresponds to primary pairing,
*       so simply copy it.

         ELSE IF (PRTYP .EQ. 'P') THEN
            OPAIR = NPAIR

            DO LOOP = 1, NPAIR
               OPRMPR(LOOP) = PRMPR(LOOP)
               OSECPR(LOOP) = SECPR(LOOP)
               OSEPN(LOOP)  = SEPN(LOOP)
               OPMULT(LOOP) = PMULT(LOOP)
               OSMULT(LOOP) = SMULT(LOOP)
            END DO

*
*       Mosaic pairing; copy the input list corresponding to all the
*       primary objects.  Determine the unpaired secondary objects
*       and append them to the output list.

         ELSE IF (PRTYP .EQ. 'M') THEN

*
*          Copy all the primary objects.

            OPAIR = NPAIR

            DO LOOP = 1, NPAIR
               OPRMPR(LOOP) = PRMPR(LOOP)
               OSECPR(LOOP) = SECPR(LOOP)
               OSEPN(LOOP)  = SEPN(LOOP)
               OPMULT(LOOP) = PMULT(LOOP)
               OSMULT(LOOP) = SMULT(LOOP)
            END DO

*
*          Determine the unpaired secondary objects.

            DO LOOP = 1, SECROW
               SECWRK(LOOP) = CAT__UPAIR
            END DO

            DO LOOP = 1, NPAIR
               IF (SECPR(LOOP) .NE. CAT__UPAIR) THEN
                  SECWRK(SECPR(LOOP) ) = 1
               END IF
            END DO

*
*          Append the unpaired secondary objects to the output list.

            DO LOOP = 1, SECROW
               IF (SECWRK(LOOP) .EQ. CAT__UPAIR) THEN
                  OPAIR = OPAIR + 1

                  OPRMPR(OPAIR) = CAT__UPAIR
                  OSECPR(OPAIR) = LOOP
                  OSEPN(OPAIR)  = UDIST
                  OPMULT(OPAIR) = 0
                  OSMULT(OPAIR) = 0
               END IF
            END DO

*
*       Primrej pairing; extract the unpaired objects in the input list.

         ELSE IF (PRTYP .EQ. 'R') THEN
            OPAIR = 0

            DO LOOP = 1, NPAIR
               IF (SECPR(LOOP) .EQ. CAT__UPAIR) THEN
                  OPAIR = OPAIR + 1

                  OPRMPR(OPAIR) = PRMPR(LOOP)
                  OSECPR(OPAIR) = SECPR(LOOP)
                  OSEPN(OPAIR)  = SEPN(LOOP)
                  OPMULT(OPAIR) = PMULT(LOOP)
                  OSMULT(OPAIR) = SMULT(LOOP)
               END IF
            END DO

*
*       Allrej pairing; extract the unpaired objects in the input list.
*       Determine the unpaired secondary objects and append them to the
*       output list.

         ELSE IF (PRTYP .EQ. 'A') THEN

*
*          Extract the unpaired objects in the input list.

            OPAIR = 0

            DO LOOP = 1, NPAIR
               IF (SECPR(LOOP) .EQ. CAT__UPAIR) THEN
                  OPAIR = OPAIR + 1

                  OPRMPR(OPAIR) = PRMPR(LOOP)
                  OSECPR(OPAIR) = SECPR(LOOP)
                  OSEPN(OPAIR)  = SEPN(LOOP)
                  OPMULT(OPAIR) = PMULT(LOOP)
                  OSMULT(OPAIR) = SMULT(LOOP)
               END IF
            END DO

*
*          Determine the unpaired secondary objects.

            DO LOOP = 1, SECROW
               SECWRK(LOOP) = CAT__UPAIR
            END DO

            DO LOOP = 1, NPAIR
               IF (SECPR(LOOP) .NE. CAT__UPAIR) THEN
                  SECWRK(SECPR(LOOP) ) = 1
               END IF
            END DO

*
*          Append the unpaired secondary objects to the output list.

            DO LOOP = 1, SECROW
               IF (SECWRK(LOOP) .EQ. CAT__UPAIR) THEN
                  OPAIR = OPAIR + 1

                  OPRMPR(OPAIR) = CAT__UPAIR
                  OSECPR(OPAIR) = LOOP
                  OSEPN(OPAIR)  = UDIST
                  OPMULT(OPAIR) = 0
                  OSMULT(OPAIR) = 0
               END IF
            END DO

*
*       The code for the type of pairing is illegal.  Set the status
*       and report an error.

         ELSE
            STATUS = SAI__ERROR

            CALL MSG_SETC ('PRTYP', PRTYP)
            CALL ERR_REP ('CAP_PAIRT_ITP', 'CAP_PAIRT: illegal '/
     :        /'code for type of pairing: ^PRTYP.', STATUS)

         END IF

C        print2000, opair
C2000    format(1x, 'opair: ', i10 / )

C        do loop = 1, opair
C           print2001, loop, oprmpr(loop), osecpr(loop)
C2001       format(1x, 'loop, oprmpr, osecpr: ', i8, i8, i8 )
C        end do

      END IF

      END
