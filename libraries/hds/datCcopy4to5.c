
/* Copy version 4 objects to a version 5 file */

/* We define X to be 4 and Y to be 5
   where X is the source and Y is the target */

#define datCopyXtoY datCopy4to5
#define datCcopyXtoY datCcopy4to5

#define datStruc_vX datStruc_v4
#define datShape_vX datShape_v4
#define datType_vX datType_v4
#define datState_vX datState_v4

#define datNew_vY datNew_v5
#define datFind_vY datFind_v5

#include "datCcopyXtoY.c"
