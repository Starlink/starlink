      SUBROUTINE REPSYS (LUREP, KRES, KSYS, JBEQ, EQ, JBEP, EP)
*+
*
*  REPSYS:  subroutine of COCO utility which reports a
*           coordinate system.
*
*  Given:
*     LUREP      int       unit number for report
*     KRES       char      report resolution: L,M or H
*     KSYS       char      system: 4,B,5,A,E or G (see commands)
*     JBEQ,EQ    char,dp   J or B prefix, equinox
*     JBEP,EP    char,dp   J or B prefix, epoch
*
*  Called:
*     sla_EPJ2D, sla_EPCO, sla_DJCAL
*
*  P T Wallace   Starlink   21 June 1995
*-

      IMPLICIT NONE

      INTEGER LUREP
      CHARACTER KRES,KSYS,JBEQ
      DOUBLE PRECISION EQ
      CHARACTER JBEP
      DOUBLE PRECISION EP

      DOUBLE PRECISION sla_EPCO,sla_EPJ2D

      INTEGER IYMDF(4),J



*  Format according to coordinate system and report resolution
               IF (KSYS.EQ.'4') THEN
                  IF (KRES.EQ.'L') THEN
                     WRITE (LUREP,'(7X,''FK4, equinox '',A,'//
     :                   'F6.1,'', epoch '',A,F6.1,'' (barycentric)'')')
     :                                                   JBEQ,EQ,JBEP,EP
                  ELSE IF (KRES.EQ.'M') THEN
                     WRITE (LUREP,'(7X,''FK4, equinox '',A,'//
     :       'F6.1,'', epoch '',A,F7.2,'' (barycentric)'')')
     :                               JBEQ,EQ,JBEP,EP
                  ELSE
                     WRITE (LUREP,'(7X,''FK4, equinox '',A,'//
     :                   'F6.1,'', epoch '',A,F8.3,'' (barycentric)'')')
     :                                                   JBEQ,EQ,JBEP,EP
                  END IF
               ELSE IF (KSYS.EQ.'B') THEN
                  IF (KRES.EQ.'L') THEN
                     WRITE (LUREP,
     :                       '(7X,''BN mean (no E-terms), equinox '','//
     :                 'A,F6.1,'', epoch '',A,F6.1,'' (barycentric)'')')
     :                                                   JBEQ,EQ,JBEP,EP
                  ELSE IF (KRES.EQ.'M') THEN
                     WRITE (LUREP,
     :                       '(7X,''BN mean (no E-terms), equinox '','//
     :                 'A,F6.1,'', epoch '',A,F7.2,'' (barycentric)'')')
     :                                                   JBEQ,EQ,JBEP,EP
                  ELSE
                     WRITE (LUREP,
     :                       '(7X,''BN mean (no E-terms), equinox '','//
     :                 'A,F6.1,'', epoch '',A,F8.3,'' (barycentric)'')')
     :                                                   JBEQ,EQ,JBEP,EP
                  END IF
               ELSE IF (KSYS.EQ.'5') THEN
                  IF (KRES.EQ.'L') THEN
                     WRITE (LUREP,'(7X,''FK5, equinox '',A,'//
     :                   'F6.1,'', epoch '',A,F6.1,'' (barycentric)'')')
     :                                                   JBEQ,EQ,JBEP,EP
                  ELSE IF (KRES.EQ.'M') THEN
                     WRITE (LUREP,'(7X,''FK5, equinox '',A,'//
     :                   'F6.1,'', epoch '',A,F7.2,'' (barycentric)'')')
     :                                                   JBEQ,EQ,JBEP,EP
                  ELSE
                     WRITE (LUREP,'(7X,''FK5, equinox '',A,'//
     :                   'F6.1,'', epoch '',A,F8.3,'' (barycentric)'')')
     :                                                   JBEQ,EQ,JBEP,EP
                  END IF
               ELSE IF (KSYS.EQ.'A') THEN
                  IF (KRES.EQ.'L') THEN
                     CALL sla_DJCAL(0,sla_EPJ2D(sla_EPCO('J',JBEP,EP)),
     :                                                          IYMDF,J)
                     IF (J.NE.0) WRITE (LUREP,'(7X,''REPSYS bug!'')')
                     WRITE (LUREP,
     :               '(7X,''Apparent (geocentric), epoch'',I5.4,2I3.2)')
     :                                                  (IYMDF(J),J=1,3)
                  ELSE IF (KRES.EQ.'M') THEN
                     CALL sla_DJCAL(1,sla_EPJ2D(sla_EPCO('J',JBEP,EP)),
     :                                                          IYMDF,J)
                     IF (J.NE.0) WRITE (LUREP,'(7X,''REPSYS bug!'')')
                     WRITE (LUREP,
     :'(7X,''Apparent (geocentric), epoch'',I5.4,2I3.2,''.'',I1)') IYMDF
                  ELSE
                     CALL sla_DJCAL(3,sla_EPJ2D(sla_EPCO('J',JBEP,EP)),
     :                                                          IYMDF,J)
                     IF (J.NE.0) WRITE (LUREP,'(1X,''REPSYS bug!'')')
                     WRITE (LUREP,
     :    '(7X,''Apparent (geocentric), epoch'',I5.4,2I3.2,''.'',I3.3)')
     :                                                             IYMDF
                  END IF
               ELSE IF (KSYS.EQ.'E') THEN
                  IF (KRES.EQ.'L') THEN
                     WRITE (LUREP,
     :    '(7X,''Ecliptic, epoch '',A,F6.1,'' (barycentric)'')') JBEP,EP
                  ELSE IF (KRES.EQ.'M') THEN
                     WRITE (LUREP,
     :    '(7X,''Ecliptic, epoch '',A,F9.4,'' (barycentric)'')') JBEP,EP
                  ELSE
                     WRITE (LUREP,
     :   '(7X,''Ecliptic, epoch '',A,F12.7,'' (barycentric)'')') JBEP,EP
                  END IF
               ELSE
                  IF (KRES.EQ.'L') THEN
                     WRITE (LUREP,
     :    '(7X,''Galactic, epoch '',A,F6.1,'' (barycentric)'')') JBEP,EP
                  ELSE IF (KRES.EQ.'M') THEN
                     WRITE (LUREP,
     :    '(7X,''Galactic, epoch '',A,F7.2,'' (barycentric)'')') JBEP,EP
                  ELSE
                     WRITE (LUREP,
     :    '(7X,''Galactic, epoch '',A,F8.3,'' (barycentric)'')') JBEP,EP
                  END IF
               END IF

      END
