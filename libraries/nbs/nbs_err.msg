.TITLE       NBS
.FACILITY    NBS,1802/PREFIX=NBS__


.SEVERITY	INFORMATIONAL

.BASE		100
SECTIONEXISTED	<noticeboard already existed>

.SEVERITY	WARNING

.BASE		200
TOOMANYDIMS	<more dimensions than maximum allowed>
TOOMANYBYTES	<more bytes than maximum allowed>
BADOFFSET	<offset is less than zero>
BADOPTION	<illegal parameter / item name>
DATANOTSAVED	<data part of noticeboard not saved - cannot restore it>

.SEVERITY	ERROR

.BASE		300
DEFINING	<currently defining noticeboard contents>
NOTDEFINING	<not currently defining noticeboard contents>
NILSID	<NIL static ID>
NILID	<NIL item ID>
PRIMITIVE	<item is primitive>
NOTPRIMITIVE	<item is not primitive>
ITEMNOTFOUND	<item does not exist>
SECTIONNOTFOUND	<noticeboard does not exist>
CANTOPEN	<can't open noticeboard definition file>
CANTWRITE	<can't write noticeboard definition file>
CANTREAD	<can't read noticeboard definition file>
NOTOWNER	<non-owner attempted to alter noticeboard>
TIMEOUT	<time out finding noticeboard or getting item value or shape>
DATASAVED	<data part of noticeboard saved - cannot restore definition>
DATANOTRESTORED	<data was not restored from noticeboard file - cannot save it>
HASIDS	<item / noticeboard has items derived from it - cannot lose it>
NOTTOPLEVEL	<item is not top-level (ie not noticeboard) - cannot lose it>
TOPLEVEL	<item is top-level (ie noticeboard) - cannot lose it>
NEVERFOUND	<parent has no items derived from it - cannot lose it>

.SEVERITY	SEVERE

.BASE		400
INITALLOCFAILED	<couldn't initialise storage allocator>
NOMOREROOM	<couldn't get memory - increase MAX_DEFN_SIZE if when defining>
BADVERSION	<noticeboard or definition file had wrong version>
IMPOSSIBLE	<something impossible happened - system error>
.END
