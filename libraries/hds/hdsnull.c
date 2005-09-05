#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*======================================*/
/* HDS_NULL - Replace obsolete routines */
/*======================================*/

/* These routines do nothing. They are intended to replace routines which have
   been removed from the VMS transfer vector.	*/

void HDS_PROFL()
{
   return;
}

void PRO_DISABLE()
{
   return;
}

void PRO_ENABLE()
{
   return;
}

void PRO_ENTER()
{
   return;
}

void PRO_EVENT()
{
   return;
}

void PRO_EXIT()
{
   return;
}

void PRO_REPEVENT()
{
   return;
}

void PRO_REPLEVEL()
{
   return;
}

void PRO_START()
{
   return;
}

void PRO_STOP()
{
   return;
}
