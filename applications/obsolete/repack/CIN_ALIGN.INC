*+CIN_ALIGN Structure to hold alignment data
      STRUCTURE /CIN_ALIGN/
* Original M.J. Ricketts
* Modified for CALLIB M. Denby
*-
         CHARACTER*10 ISSUE_DATE
         INTEGER VERSION_NUMBER
         DOUBLE PRECISION MJD			! Date for which data requested
         UNION
            MAP
               RECORD/ALIGN_REC/ ROSAT_TO_WFC
               RECORD/ALIGN_REC/ WFC_TO_ST
               RECORD/ALIGN_REC/ WFC_TO_FOV
               RECORD/ALIGN_REC/ ROSAT_TO_ROST(2)
            END MAP
            MAP
               RECORD/ALIGN_REC/ ALIGNMENT(5)
            END MAP
         END UNION
      END STRUCTURE
      RECORD /CIN_ALIGN/ CAL
      STRUCTURE /NEW_ALIGN/
         CHARACTER*20 SET
         RECORD/ALIGN_REC/ REC
      END STRUCTURE
      RECORD/NEW_ALIGN/ ALIGN_NEW
