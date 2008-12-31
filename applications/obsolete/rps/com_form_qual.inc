*+COM_FORM_QUAL    Include file, holds Qual check data for cover, targets
      INTEGER QTARGET			! Target Number
      LOGICAL QUAL_COVER		! True of Cover OK
      LOGICAL QUAL_GEN			! True of general OK
      LOGICAL QUAL_TARGET(90)		! True if Target(qtarget) OK
 
      COMMON /COM_FORM_QUAL/ QTARGET,QUAL_COVER,QUAL_GEN,QUAL_TARGET
