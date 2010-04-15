C# IL>=a, OL>=0
      SUBROUTINE GKPSAD (IWN,ISI,IWI,NSI,IWSI,NSO,IWSO)
*
* (C) COPYRIGHT ICL & SERC  1989
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             NGB/KEVP
*
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Manipulate a section list for single polygon so as to
*     add a vertex to a polygon BEFORE a specified vertex
*     and make the polygon start and end at this new vertex.
*
*
*  MAINTENANCE LOG
*  ---------------
*     23/02/89  KEVP  Stabilised
*     18/04/89  KEVP  Corrected handling of reversed sections.
*     26/04/89  KEVP  Removed debug WRITE statements.
*     22/07/90  PLP  Commenting brought in line with standard format;
*                    also removed unused locals NADDS and IS.
*
*  ARGUMENTS
*  ---------
*     INP  IWN   Index of new vertex to be added
*     INP  ISI   Section of specified vertex
*     INP  IWI   Index of specified vertex
*     INP  NSI   Original Number of sections
*     INP  IWSI  Original Section List for 1 polygon
*     OUT  NSO   Final Number of sections
*     OUT  IWSO  Final Section List for 1 polygon
*
      INTEGER  IWN, ISI,IWI, NSI, IWSI(2,NSI), NSO, IWSO(2,NSI+2)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkerr.cmn'
*
*
*  LOCALS
*  ------
*     IWSL    lowermost index of section
*     IWSU    uppermost index of section
*     ISFORM  index from which section is moved
*     ISTO    index to which section is moved
*
      INTEGER ISFROM, ISTO, IWSL,IWSU
*
*  STACK USAGE
*  -----------
*     none
*
*  COMMENTS
*  --------
*     The start of the polygon is moved to the new vertex,
*     so that it can easily be joined to other such polygons.
*
*     MSA must be at least NSECT+2.
*
*  ERRORS
*  ------
*     -2004 Vertex is not in its section.
*
*  ALGORITHM
*  ---------
*
*----------------------------------------------------------------------
*
      IF(IWSI(1,ISI) .LT. IWSI(2,ISI)) THEN
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*        Forward Section - insert below
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         IWSL = IWSI(1,ISI)
         IWSU = IWSI(2,ISI) - 1
*        Check that specified vertex is in the Section
         IF((IWSL .GT. IWI) .OR. (IWI .GT. IWSU)) THEN
           CALL GKBUG(-2004,'GKPSAD')
         ELSE
*        Specified Vertex found in the Section
*          Add new vertex to section list
           IF(IWSL .EQ. IWI)THEN
*            New vertex inserted before section
             NSO = NSI + 1
             ISFROM   = ISI
           ELSE
*            New vertex inserted within section - So Splitting it
             NSO = NSI + 2
             IWSO(1,1) = IWI
             IWSO(2,1) = IWSU + 1
             IWSO(1,NSI+1) = IWSL
             IWSO(2,NSI+1) = IWI
             ISFROM   = ISI + 1
           ENDIF

*          Move sections
           DO 20 ISTO=NSO-NSI,NSI
             IF(ISFROM .GT. NSI)ISFROM = ISFROM - NSI
             IWSO(1,ISTO) = IWSI(1,ISFROM)
             IWSO(2,ISTO) = IWSI(2,ISFROM)
             ISFROM = ISFROM + 1
   20      CONTINUE

*          Add New Vertex
           IWSO(1,NSO) = IWN
           IWSO(2,NSO) = IWN + 1
         ENDIF

      ELSEIF(IWSI(1,ISI) .GT. IWSI(2,ISI)) THEN
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
*        Reversed Section - insert above
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         IWSL = IWSI(2,ISI)
         IWSU = IWSI(1,ISI) - 1
*        Check that specified vertex is in the Section
         IF((IWSL .GT. IWI) .OR. (IWI .GT. IWSU)) THEN
           CALL GKBUG(-2004,'GKPSAD')
         ELSE
*        Specified Vertex found in the Section
*          Add new vertex to section list
           IF(IWSU .EQ. IWI)THEN
*            New vertex inserted before section
             NSO = NSI + 1
             ISFROM   = ISI
           ELSE
*            New vertex inserted within section - So Splitting it
             NSO = NSI + 2
             IWSO(1,1) = IWI + 1
             IWSO(2,1) = IWSL
             IWSO(1,NSI+1) = IWSU + 1
             IWSO(2,NSI+1) = IWI + 1
             ISFROM = ISI + 1
           ENDIF

*          Move sections
           DO 30 ISTO=NSO-NSI,NSI
             IF(ISFROM .GT. NSI)ISFROM = ISFROM - NSI
             IWSO(1,ISTO) = IWSI(1,ISFROM)
             IWSO(2,ISTO) = IWSI(2,ISFROM)
             ISFROM = ISFROM + 1
   30      CONTINUE

*          Add New Vertex
           IWSO(1,NSO) = IWN
           IWSO(2,NSO) = IWN + 1
         ENDIF

      ELSE
*0000000000000000000000000000000000000000000000000000000000
*Zero length section - specified vertex can't belong to it!
*0000000000000000000000000000000000000000000000000000000000
        CALL GKBUG(-2004,'GKPSAD')
      ENDIF

      END
