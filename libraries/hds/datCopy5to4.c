
/* Copy version 5 objects to a version 4 file */

/* We define X to be 5 and Y to be 4
   where X is the source and Y is the target */

#define datCopyXtoY datCopy5to4

#define datStruc_vX datStruc_v5
#define datShape_vX datShape_v5
#define datType_vX datType_v5
#define datMap_vX datMap_v5
#define datUnmap_vX datUnmap_v5
#define datLen_vX datLen_v5
#define datIndex_vX datIndex_v5
#define datAnnul_vX datAnnul_v5
#define datVec_vX datVec_v5
#define datCell_vX datCell_v5
#define datName_vX datName_v5
#define datNcomp_vX datNcomp_v5

#define datNew_vY datNew_v4
#define datFind_vY datFind_v4
#define datMap_vY datMap_v4
#define datUnmap_vY datUnmap_v4
#define datLen_vY datLen_v4
#define datAnnul_vY datAnnul_v4
#define datVec_vY datVec_v4
#define datCell_vY datCell_v4

#include "datCopyXtoY.c"
