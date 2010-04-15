      SUBROUTINE SCULIB_SET_QUAL (USE_SECT, QUALITY, N_BOLS, N_POS,
     :     N_BEAM, BOL_S, POS_S, BIT_POS, BIT_SWITCH, STATUS)
*+
*  Name:
*     SCULIB_SET_QUAL

*  Purpose:
*     set quality bits in a subset of a quality array

*  Description:
*     Set the bits in the quality array as specified by a mask.
*     Can be used to set or unset bits as well as using the mask
*     or the inverse of the mask. This is used to mask scuba sections.

*  Invocation:
*     CALL SCULIB_SET_QUAL (USE_SECT, QUALITY, N_BOLS, N_POS, N_BEAM,
*    :  BOL_S, POS_S, BIT_POS, BIT_SWITCH, STATUS)

*  Arguments:
*     USE_SECT                = LOGICAL (Given)
*           am I changing SECTION or .NOT. SECTION
*     QUALITY (N_BOLS, N_POS, N_BEAM) = BYTE (Given and returned)
*           the quality array
*     N_BOLS                  = INTEGER (Given)
*           number of bolometers measured
*     N_POS                   = INTEGER (Given)
*           number of positions measured
*     N_BEAM                  = INTEGER (Given)
*           number of beams used
*     BOL_S (N_BOLS)          = INTEGER (Given)
*           array containing 1s for bolometers whose quality is to be
*           changed
*     POS_S (N_POS)           = INTEGER (Given)
*           array containing 1s for positions whose quality is to be
*           changed
*     BIT_POS                 = INTEGER (Given)
*           position of bit ot be set (0 - 7)
*     BIT_SWITCH               = LOGICAL (Given)
*           If .TRUE. we set the bit using SCULIB_BITON. If .FALSE.
*           we unset the bit with SCULIB_BITOFF
*     STATUS                  = INTEGER (Given and returned)
*           global status


*  Authors:
*     J.Lightfoot (jfl@roe.ac.uk)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Log$
*     Revision 1.6  1999/08/19 03:37:25  timj
*     Header tweaks to ease production of SSN72 documentation.
*


*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER N_BOLS
      INTEGER N_POS
      INTEGER N_BEAM
      INTEGER BOL_S (N_BOLS)
      INTEGER POS_S (N_POS)
      INTEGER BIT_POS
      LOGICAL BIT_SWITCH
      LOGICAL USE_SECT

*  Arguments Given & Returned:
      BYTE QUALITY (N_BOLS, N_POS, N_BEAM)

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  External references:
      BYTE SCULIB_BITOFF                ! clear a bit
      BYTE SCULIB_BITON                 ! set a bit

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER BEAM                      ! beam index in DO loop
      INTEGER BOL                       ! bolometer index in DO loop
      INTEGER POS                       ! measured position index in DO loop

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  set the quality

      IF (USE_SECT) THEN

         IF (BIT_SWITCH) THEN

            DO POS = 1, N_POS
               IF (POS_S(POS) .EQ. 1) THEN

                  DO BOL = 1, N_BOLS
                     IF (BOL_S(BOL) .EQ. 1) THEN

                        DO BEAM = 1, N_BEAM
                           QUALITY (BOL,POS,BEAM) =
     :                          SCULIB_BITON (QUALITY(BOL,POS,BEAM),
     :                          BIT_POS)
                        END DO

                     END IF
                  END DO

               END IF
            END DO

         ELSE

            DO POS = 1, N_POS
               IF (POS_S(POS) .EQ. 1) THEN

                  DO BOL = 1, N_BOLS
                     IF (BOL_S(BOL) .EQ. 1) THEN

                        DO BEAM = 1, N_BEAM
                           QUALITY (BOL,POS,BEAM) =
     :                          SCULIB_BITOFF (QUALITY(BOL,POS,BEAM),
     :                          BIT_POS)
                        END DO

                     END IF
                  END DO

               END IF
            END DO

         END IF
      ELSE
*     This is the inverse section

         IF (BIT_SWITCH) THEN
*     Setting the bit
            DO POS = 1, N_POS

               IF (POS_S(POS) .EQ. 0) THEN
*     We know that this entire row is unmasked
                  DO BOL = 1, N_BOLS
                     DO BEAM = 1, N_BEAM
                        QUALITY (BOL,POS,BEAM) =
     :                       SCULIB_BITON (QUALITY(BOL,POS,BEAM),
     :                       BIT_POS)
                     END DO
                  END DO
               ELSE
*     Only set if unmasked
                  DO BOL = 1, N_BOLS
                     IF (BOL_S(BOL) .EQ. 0) THEN
                        DO BEAM = 1, N_BEAM
                           QUALITY (BOL,POS,BEAM) =
     :                          SCULIB_BITON (QUALITY(BOL,POS,BEAM),
     :                          BIT_POS)
                        END DO
                     END IF
                  END DO
               END IF
            END DO

         ELSE
*     Unsetting a bit
            DO POS = 1, N_POS

               IF (POS_S(POS) .EQ. 0) THEN
*     We know that this entire row is unmasked
                  DO BOL = 1, N_BOLS
                     DO BEAM = 1, N_BEAM
                        QUALITY (BOL,POS,BEAM) =
     :                       SCULIB_BITOFF (QUALITY(BOL,POS,BEAM),
     :                       BIT_POS)
                     END DO
                  END DO
               ELSE
*     Only set if unmasked
                  DO BOL = 1, N_BOLS
                     IF (BOL_S(BOL) .EQ. 0) THEN
                        DO BEAM = 1, N_BEAM
                           QUALITY (BOL,POS,BEAM) =
     :                          SCULIB_BITOFF (QUALITY(BOL,POS,BEAM),
     :                          BIT_POS)
                        END DO
                     END IF
                  END DO
               END IF
            END DO


         END IF

      END IF

      END
