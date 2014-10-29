
/* Copy version 4 objects to a version 5 file */

/* We define X to be 4 and Y to be 5
   where X is the source and Y is the target */

#define datCopyXtoY datCopy4to5

#define datStruc_vX datStruc_v4
#define datShape_vX datShape_v4
#define datType_vX datType_v4
#define datMap_vX datMap_v4
#define datUnmap_vX datUnmap_v4
#define datLen_vX datLen_v4
#define datIndex_vX datIndex_v4
#define datAnnul_vX datAnnul_v4
#define datVec_vX datVec_v4
#define datCell_vX datCell_v4
#define datName_vX datName_v4
#define datNcomp_vX datNcomp_v4

#define datNew_vY datNew_v5
#define datFind_vY datFind_v5
#define datMap_vY datMap_v5
#define datUnmap_vY datUnmap_v5
#define datLen_vY datLen_v5
#define datAnnul_vY datAnnul_v5
#define datVec_vY datVec_v5
#define datCell_vY datCell_v5

#include "datCopyXtoY.c"
