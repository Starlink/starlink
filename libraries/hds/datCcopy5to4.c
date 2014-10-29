
/* Copy version 5 objects to a version 4 file */

/* We define X to be 5 and Y to be 4
   where X is the source and Y is the target */

#define datCopyXtoY datCopy5to4
#define datCcopyXtoY datCcopy5to4

#define datStruc_vX datStruc_v5
#define datShape_vX datShape_v5
#define datType_vX datType_v5
#define datState_vX datState_v5

#define datNew_vY datNew_v4
#define datFind_vY datFind_v4

#include "datCcopyXtoY.c"
