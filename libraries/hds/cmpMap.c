#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "hds.h"
#include "dat_par.h"
#include "cmp_err.h"

/* List of mapped components. */

struct cmpMapped {
    HDSLoc* struc;
    char name[DAT__SZNAM + 1];
    HDSLoc* loc;
    struct cmpMapped* next;
};

static struct cmpMapped* _mapped = 0;

static struct cmpMapped* _mappedFind(HDSLoc* struc, const char* name) {
    struct cmpMapped* p;
    for (p = _mapped; p; p = p->next) {
        if ((p->struc == struc) && ! strncasecmp(p->name, name, DAT__SZNAM)) {
            return p;
        }
    }
    return 0;
}

static void _mappedAdd(HDSLoc* struc, const char* name, HDSLoc* loc) {
    struct cmpMapped* p = malloc(sizeof(struct cmpMapped));
    p->struc = struc;
    strncpy(p->name, name, DAT__SZNAM);
    p->name[DAT__SZNAM] = '\0';
    p->loc = loc;
    p->next = _mapped;
    _mapped = p;
}

static void _mappedRemove(HDSLoc* struc, const char* name) {
    struct cmpMapped** q = &_mapped;
    struct cmpMapped* p;
    for (p = *q; p; q = &p->next, p = *q) {
        if ((p->struc == struc) && ! strncasecmp(p->name, name, DAT__SZNAM)) {
            *q = p->next;
            free(p);
            break;
        }
    }
}

/*
*+
*  Name:
*     cmpMapV

*  Purpose:
*     Map component as if it were a vector.

*  Synopsis:
*     void cmpMapV(HDSLoc* struc, const char *name, const char *type,
                   const char *mode, void **pntr, size_t *actval, int *status)

*  Description:
*     This routine maps a primitive component of a structure for
*     reading, writing or updating, as if it were vectorized.
*     The number of values mapped is returned in the variable,
*     actval.

*  Arguments:
*     struc
*        Locator associated with a structured data object.
*     name
*        The component name of a primitive object contained in the structure.
*     type
*        Expression specifying the data type of the mapped values.
*        If the actual type of the data object differs from this,
*        then conversion will be performed in 'READ' and 'UPDATE'
*        modes.
*     mode
*        Expression specifying the mode in which the data are to be
*        mapped.  (Either 'READ', 'WRITE' or 'UPDATE'.)
*     pntr
*        Variable to receive the pointer for the mapped values.
*     actval
*        Variable to receive the number of values mapped.
*     status
*        Variable holding the status value.

*  Authors:
*     GSB: Graham Bell (EAO)

*  History:
*     08-MAY-2019 (GSB):
*        Original version, based on equivalent Fortran routine.

*-
*/

void cmpMapV(HDSLoc* struc, const char *name, const char *type,
             const char *mode, void **pntr, size_t *actval, int *status) {
    HDSLoc *loc = NULL;
    if (*status != SAI__OK) {
        return;
    }

    if (_mappedFind(struc, name)) {
        *status = CMP__ISMAP;
        emsRep("", "cmpMapV: the named component is already mapped", status);
        return;
    }

    datFind(struc, name, &loc, status);

    if (*status == SAI__OK) {
        datMapV(loc, type, mode, pntr, actval, status);
        if (*status != SAI__OK) {
            datAnnul(&loc, status);
        }
        else {
            _mappedAdd(struc, name, loc);
        }
    }
}

/*
*+
*  Name:
*     cmpMapN

*  Purpose:
*     Map component as n-dimensional array.

*  Synopsis:
*     void cmpMapN(HDSLoc* struc, const char *name, const char *type,
                   const char *mode, int ndim, void **pntr, hdsdim dims[], int *status)

*  Description:
*     This routine maps a primitive component of a structure for
*     reading, writing or updating.   The caller is expected to know
*     the number of object dimensions, ndim.   The object dimensions
*     are returned in the array, dims.

*  Arguments:
*     struc
*        Locator associated with a structured data object.
*     name
*        The component name of a primitive object contained in the structure.
*     type
*        Expression specifying the data type of the mapped values.
*        If the actual type of the data object differs from this,
*        then conversion will be performed in 'READ' and 'UPDATE'
*        modes.
*     mode
*        Expression specifying the mode in which the data are to be
*        mapped.  (Either 'READ', 'WRITE' or 'UPDATE'.)
*     ndim
*        Expression specifying the number of array dimensions.
*        This must match the actual number of object dimensions.
*     pntr
*        Variable to receive the pointer for the mapped values.
*     dims
*        Array to receive the dimensions of the object mapped.
*     status
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Authors:
*     GSB: Graham Bell (EAO)

*  History:
*     08-MAY-2019 (GSB):
*        Original version, based on equivalent Fortran routine.

*-
*/

void cmpMapN(HDSLoc* struc, const char *name, const char *type,
             const char *mode, int ndim, void **pntr, hdsdim dims[], int *status) {
    HDSLoc *loc = NULL;
    if (*status != SAI__OK) {
        return;
    }

    if (_mappedFind(struc, name)) {
        *status = CMP__ISMAP;
        emsRep("", "cmpMapN: the named component is already mapped", status);
        return;
    }

    datFind(struc, name, &loc, status);

    if (*status == SAI__OK) {
        datMapN(loc, type, mode, ndim, pntr, dims, status);
        if (*status != SAI__OK) {
            datAnnul(&loc, status);
        }
        else {
            _mappedAdd(struc, name, loc);
        }
    }
}

/*
*+
*  Name:
*     cmpUnmap

*  Purpose:
*     Unmap structure component.

*  Synopsis:
*     void cmpUnmap(HDSLoc* struc, const char *name, int *status)

*  Description:
*     Unmap a structure component, previously mapped with cmpMapV
*     or cmpMapN.

*  Arguments:
*     struc
*        Locator associated with a structured data object.
*     comp
*        The component name of a primitive object contained in the structure.
*     status
*        Variable holding the status value.
*        The routine will attempt to execute regardless of the input
*        value of this variable.
*        If its input value is not SAI__OK then it is left unchanged
*        by this routine, even if it fails to complete.
*        If its input value is SAI__OK and this routine fails, then
*        the value is changed to an appropriate error number.

*  Authors:
*     GSB: Graham Bell (EAO)

*  History:
*     08-MAY-2019 (GSB):
*        Original version, based on equivalent Fortran routine.

*-
*/

void cmpUnmap(HDSLoc* struc, const char *name, int *status) {
    int istat;
    struct cmpMapped *entry;

    istat = *status;
    *status = SAI__OK;

    entry = _mappedFind(struc, name);

    if (! entry) {
        if (istat != SAI__OK) {
            *status  = istat;
            return;
        }
        else {
            *status = CMP__NOMAP;
            emsRep("", "cmpUnmap: the named component is not mapped", status);
            return;
        }
    }

    datUnmap(entry->loc, status);
    datAnnul(&entry->loc, status);

    _mappedRemove(struc, name);

    if (istat != SAI__OK) {
        *status = istat;
    }
}
