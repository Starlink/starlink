##---------------------------------------------------------------------------##
##  File:
##      @(#)  DTD.pm 1.6 97/09/15 @(#)
##  Author:
##      Earl Hood			ehood@medusa.acs.uci.edu
##  Description:
##      This file defines the SGML::DTD class.  Class is used for
##	parsing and analyzing DTDs.
##---------------------------------------------------------------------------##
##  Copyright (C) 1996,1997	Earl Hood, ehood@medusa.acs.uci.edu
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
## 
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##  
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, write to the Free Software
##  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
##---------------------------------------------------------------------------##
##  Current status of package:
##
##	o <!ATTLIST #NOTATION is ignored.
##
##      o LINKTYPE, SHORTREF, USEMAP declarations are ignored.
##
##	o Rank element declarations are not supported.
##
##---------------------------------------------------------------------------##

package SGML::DTD;

use SGML::Syntax qw(:Delims :Keywords);
use SGML::EntMan;

## Derive from Exporter
use Exporter ();
@ISA = qw(Exporter);

@EXPORT = ();
@EXPORT_OK = ();
%EXPORT_TAGS = ();
$VERSION = "0.02";

##---------------------------------------------------------------------------##
##  Object methods
##  --------------
##	get_base_children	=> Get base elements of an element
##	get_elem_attr   	=> Get attributes for an element
##	get_elements		=> Get array of all elements
##	get_elements_of_attr	=> Get array of elements that have attribute
##	get_exc_children 	=> Get exclusion elements of an element
##	get_gen_ents		=> Get general entities defined in DTD
##	get_gen_data_ents	=> Get general entities: {PC,C,S}DATA, PI
##	get_inc_children 	=> Get inclusion elements of an element
##	get_parents		=> Get parent elements of an element
##	get_top_elements 	=> Get top-most elements
##	is_attr_keyword 	=> Check for reserved attribute value
##	is_child 		=> Check if child of element
##	is_elem_keyword 	=> Check for reserved element value
##	is_element		=> Check if element defined in DTD
##	is_group_connector	=> Check for group connector
##	is_occur_indicator	=> Check for occurrence indicator
##	is_tag_name		=> Check for legal tag name.
##	print_tree		=> Output content tree for an element
##	read_dtd 		=> Parse a SGML dtd
##	reset   		=> Reset all internal data for DTD
## 
##  Class methods
##  -------------
##	set_comment_callback    => Set SGML comment callback
##	set_debug_callback	=> Set debug callback
##	set_debug_handle 	=> Set debug filehandle
##	set_ent_manager 	=> Set entity manager
##	set_err_callback 	=> Set error callback
##	set_err_handle  	=> Set error filehandle
##	set_pi_callback 	=> Set processing instruction callback
##	set_tree_callback	=> Set callback for printing a tree entry
##	set_verbosity   	=> Set verbosity flag
##---------------------------------------------------------------------------##

##***************************************************************************##
##			       CLASS VARIABLES				     ##
##***************************************************************************##
##--------------------##
## Internal variables ##
##--------------------##
$keywords = "$CDATA|$CONREF|$CURRENT|$EMPTY|$ENTITY|$ENTITIES|$FIXED|".
	    "$ID|$IDREF|$IDREFS|$IMPLIED|$NAME|$NAMES|$NDATA|$NMTOKEN|".
	    "$NMTOKENS|$NOTATION|$NUMBER|$NUMBERS|$NUTOKEN|$NUTOKENS|$PCDATA|".
	    "$RCDATA|$REQUIRED|$SDATA";

$elem_keywords = "$rni$PCDATA|$RCDATA|$CDATA|$EMPTY|$ANY";
$attr_keywords = "$CDATA|$ENTITY|$ENTITIES|$ID|$IDREF|$IDREFS|$NAME|$NAMES|".
		 "$NMTOKEN|$NMTOKENS|$NOTATION|$NUMBER|$NUMBERS|$NUTOKEN|".
		 "$NUTOKENS|$rni$FIXED|$rni$REQUIRED|$rni$CURRENT|".
		 "$rni$IMPLIED|$rni$CONREF";

##--------------##
## Function map ##
##--------------##
%Function = (
    $ATTLIST	=>	\&do_attlist,
    $ELEMENT	=>	\&do_element,
    $ENTITY	=>	\&do_entity,
    $NOTATION	=>	\&do_notation,
    $SHORTREF	=>	\&do_shortref,
    $USEMAP	=>	\&do_usemap,
);

##-------------------------##
## Miscellaneous variables ##
##-------------------------##
$Verbose	= 0;	# Flag if generating debugging output

## Entity manager
$EntMan  	= new SGML::EntMan;

## Callbacks
$CommentCallback= '';
$PICallback	= '';
$DebugHandle	= \*STDERR;
$DebugCallback	= '';
$ErrHandle	= \*STDERR;
$ErrMsgCallback	= '';

$MaxLevel = 5;		# Default tree depth (root element has depth = 1)
$TreeFile = \*STDOUT;	# Default output file
$PrTreeEntry = \&pr_tree_entry;
			# Print tree entry callback

##  Constants to determine if data read should be processed.
*IncMS  	= \1;
*IgnMS  	= \2;

##***************************************************************************##
##			       PUBLIC METHODS				     ##
##***************************************************************************##

##---------------------------------------------------------------------------##
##			 	CONSTRUCTOR				     ##
##---------------------------------------------------------------------------##
##	new() is the constructor routine for class DTD.  The constructor
##	may take a filehandle as an argument for a DTD to parse.  If
##	the parse fails, new() will return undef.
##
sub new {
    my $this = {};
    my $class = shift;
    bless $this, $class;
    $this->reset();

    ## Check if filehandle passed during construction

    my $fh = shift;
    my $status = 1;
    if ($fh) {

	# Set entity manager if passed in during construction
	my $entman = shift;
	$EntMan = $entman  if $entman;

	# Read DTD
	$status = $this->read_dtd($fh);
    }

    ## Return object

    $status ? $this : undef;
}

##---------------------------------------------------------------------------##
##			 DATA ACCESS/UTILITY METHODS			     ##
##---------------------------------------------------------------------------##

##---------------------------------------------------------------------------
##	get_elements() retrieves all the elements defined in the DTD.
##	An optional flag argument can be passed to the routine to
##	determine if elements returned are sorted or not: 0 => sorted,
##	1 => not sorted.
##
sub get_elements {
    my $this = shift;

    my($nosort) = shift;

    $nosort ? @{$this->{Elements}} : sort keys %{$this->{ElemCont}};
}

##---------------------------------------------------------------------------
##	get_elements_of_attr() retrieves all the elements that
##	have the attribute $attr.
##
sub get_elements_of_attr {
    my $this = shift;

    my($attr) = shift;

    sort @{$this->{ElemsOfAttr}{lc $attr}};
}

##---------------------------------------------------------------------------
##	get_elem_attr() retrieves an associative array defining the
##	attributes associated with element $elem.
##
sub get_elem_attr {
    my $this = shift;
    my $elem = shift;

    %{$this->{Attribute}{lc $elem}};
}

##---------------------------------------------------------------------------
##	get_top_elements() retrieves the top-most elements in the DTD.
##
sub get_top_elements {
    my $this = shift;

    $this->compute_parents();
    sort keys %{$this->{TopElement}};
}

##---------------------------------------------------------------------------
##	get_parents() returns an array of elements that can be parent
##	elements of $elem.
##
sub get_parents {
    my $this = shift;
    my $elem = shift;

    $this->compute_parents();
    sort @{$this->{Parents}{lc $elem}};
}

##---------------------------------------------------------------------------
##	get_base_children() returns an array of the elements in
##	the base model group of $elem.
##
##	The $andcon is flag if the connector characters are included
##	in the array.
##
sub get_base_children {
    my $this = shift;
    my($elem, $andcon) = @_;

    &extract_elem_names($this->{ElemCont}{lc $elem}, $andcon);
}

##---------------------------------------------------------------------------
##	get_inc_children() returns an array of the elements in
##	the inclusion group of $elem content rule.
##
sub get_inc_children {
    my $this = shift;
    my($elem, $andcon) = @_;

    &extract_elem_names($this->{ElemInc}{lc $elem}, $andcon);
}

##---------------------------------------------------------------------------
##	get_exc_children() returns an array of the elements in
##	the exclusion group of $elem content rule.
##
sub get_exc_children {
    my $this = shift;
    my($elem, $andcon) = @_;

    &extract_elem_names($this->{ElemExc}{lc $elem}, $andcon);
}

##---------------------------------------------------------------------------
##	get_gen_ents() returns an array of general entities.
##	An optional flag argument can be passed to the routine to
##	determine if entities returned are sorted or not: 0 => sorted,
##	1 => not sorted.
##
sub get_gen_ents {
    my $this = shift;
    my $nosort = shift;

    $nosort ? @{$this->{GenEntities}} : sort @{$this->{GenEntities}};
}

##---------------------------------------------------------------------------
##	get_gen_data_ents() returns an array of general data
##	entities defined in the DTD.  Data entities cover the
##	following: PCDATA, CDATA, SDATA, PI.
##
sub get_gen_data_ents {
    my $this = shift;

    sort keys %{$this->{GenEntity}},		# PCDATA
	 keys %{$this->{PIEntity}},		# PI
	 keys %{$this->{CDataEntity}},		# CDATA
	 keys %{$this->{SDataEntity}};		# SDATA
}

##---------------------------------------------------------------------------
##	is_attr_keyword() returns 1 if $word is an SGML reserved word
##	for an attribute value.
##
sub is_attr_keyword {
    my $class = shift;
    my $word  = shift;

    $word =~ /^\s*($attr_keywords)\s*$/oi ? 1 : 0;
}

##---------------------------------------------------------------------------
##	is_child() return 1 if $child is a child element of $elem.
##
sub is_child {
    my $this = shift;

    my($elem, $child) = @_;
    my(%tmp, $ret);

    $elem =~ tr/A-Z/a-z/;
    $child =~ tr/A-Z/a-z/;

    grep($tmp{$_}=1, &extract_elem_names($this->{ElemCont}{$elem}),
		     &extract_elem_names($this->{ElemInc}{$elem}));
    grep($tmp{$_}=0, &extract_elem_names($this->{ElemExc}{$elem}));

    $ret = $tmp{$child};
}

##---------------------------------------------------------------------------
##	is_elem_keyword() returns 1 if $word is an SGML reserved word
##	used in an element content rule.
##
sub is_elem_keyword {
    my $class = shift;
    my $word  = shift;

    $word =~ /^\s*($elem_keywords)\s*$/oi ? 1 : 0;
}

##---------------------------------------------------------------------------
##	is_element() returns 1 if passed in string is an element
##	defined in the DTD.  Else it returns zero.
##
sub is_element {
    my $this = shift;
    my $elem = shift;

    $this->{ElemCont}{lc $elem} ? 1 : 0;
}

##---------------------------------------------------------------------------
sub is_occur_indicator {
    my $class = shift;
    my $str = shift;

    $str =~ /^\s*[$plus$opt$rep]\s*$/oi ? 1 : 0;
}

##---------------------------------------------------------------------------
sub is_group_connector {
    my $class = shift;
    my $str = shift;

    $str =~ /^\s*[$seq$and$or]\s*$/oi ? 1 : 0;
}

##---------------------------------------------------------------------------
##	is_tag_name() returns 1 if $word is a legal tag name.
##
sub is_tag_name {
    my $class = shift;
    my $word = shift;

    $word =~ /^\s*[$namechars]+\s*$/oi ? 1 : 0;
}

##---------------------------------------------------------------------------
##	print_tree() outputs the tree hierarchy of $elem to the
##	filehandle specified by $handle.  $depth specifies the maximum
##	depth of the tree.
##
##      The routine cuts at elements that exist at
##      higher (or equal) levels or if $MaxLevel has been reached.  The
##      string "..." is appended to an element if has been cut-off due
##      to pre-existance at a higher (or equal) level.
##
##      Cutting the tree at repeat elements is necessary to avoid
##      a combinatorical explosion with recursive element definitions.
##      Plus, it does not make much since to repeat information.
##
sub print_tree {
    my $this = shift;
    my($elem, $depth, $handle) = @_;
    local(%inc, %exc, %done, %open, @padlen);

    $MaxLevel = $depth if ($depth > 0);
    $TreeFile = $handle if $handle;
    &print_elem($elem, 1, 1);
    $elem =~ tr/A-Z/a-z/;

    ## The following subroutines rely on the dynamic scoping of
    ## local variables defined in this routine.
    ##
    $this->compute_levels($elem, 1);	# Compute prune values
    %inc = (); %exc = (); @padlen = (0);
    $this->print_sub_tree($elem, 2);	# Print tree
}

##---------------------------------------------------------------------------
##	reset() initializes all instance variables.
##
sub reset {
    my $this = shift;

    $this->{ParEntity} 		= {}; # Int parameter entities
    $this->{PubParEntity} 	= {}; # Ext public parameter entities
    $this->{SysParEntity} 	= {}; # Ext system parameter entities
    $this->{GenEntity} 		= {}; # (pcdata) general entities
    $this->{StartTagEntity} 	= {}; # Start tag entities (STARTTAG)
    $this->{EndTagEntity} 	= {}; # End tag entities (ENDTAG)
    $this->{MSEntity} 		= {}; # Marked section ents (MS)
    $this->{MDEntity} 		= {}; # Markup declaration ents (MD)
    $this->{PIEntity} 		= {}; # Processing instructions ents (PI)
    $this->{CDataEntity} 	= {}; # Character data entities (CDATA)
    $this->{SDataEntity} 	= {}; # System data ents (SDATA)

    ## Following ent structures currently not used.
    $this->{PubEntity} 		= {}; # External public ents (PUBLIC)
    $this->{SysEntity} 		= {}; # External system ents (SYSTEM)
    $this->{SysCDEntity} 	= {}; # Ext cdata ents (SYSTEM CDATA)
    $this->{SysNDEntity} 	= {}; # Ext non-SGML ents (SYSTEM NDATA)
    $this->{SysSDEntity} 	= {}; # Ext sdata ents (SYSTEM SDATA)
    $this->{SysSubDEntity} 	= {}; # Ext sub doc ents (SYSTEM SUBDOC)

    $this->{SysNotation} 	= {}; # Notations w/SYSTEM ids
    $this->{PubNotation} 	= {}; # Notations w/PUBLIC ids

    $this->{ShortRef} 		= {}; # Short ref mappings
    $this->{UseMap} 		= {}; # Maps in use (<!USEMAP ...)

    $this->{ElemCont} 		= {}; # Element base content model
    $this->{ElemInc} 		= {}; # Element inclusions
    $this->{ElemExc} 		= {}; # Element exclusions
    $this->{ElemTag} 		= {}; # Element tag minimization
    $this->{Attribute} 		= {}; # Element attributes
    $this->{ElemsOfAttr} 	= {}; # Elements that have attribute

    $this->{ParEntities}    	= []; # Order parameter ents declared
    $this->{GenEntities}    	= []; # Order general ents declared
    $this->{Elements}       	= []; # Order elements declared

    $this->{_DidParents}	= 0;  # Flag is parents computed
    $this->{Parents}		= {}; # Parents of an element
    $this->{TopElement}		= {}; # Top-most elements

    $this->{_AGE} 		= {}; # Internal ent tracking

    $this->{DocType}		= ""; # Document type (if declared)
}

##---------------------------------------------------------------------------##
##				PARSE METHODS				     ##
##---------------------------------------------------------------------------##
##  Notes:
##	The parsing routines have a specific calling sequence.  Many
##	of the routines rely on other routines updating the current
##	parsed line.  Many of them pass the current line by reference.
##	This may look ugly, but hey, it works.
##
##	See individual routine declaration for more information.
##---------------------------------------------------------------------------

##---------------------------------------------------------------------------
##	read_dtd() parses the contents of an open file specified by
##	$handle.  A 1 is returned on successful parsing, and a 0
##	is returned if failed.  The $include argument is for internal
##	use and not meant for external routines.
##
sub read_dtd {
    my $this = shift;

    my($handle, $include) = @_;
    my($line, $c);
    my($oldslash) = $/;
    my($old) = select($handle);

    ## Eval main loop to catch fatal errors
    eval {
      DTDBLK: {
	$include = $IncMS unless $include;
	if ($include == $IgnMS) {		# Do nothing if ignoring
	    last DTDBLK;
	}
	DTDPARSELOOP: while (!eof($handle)) {
	    $/ = $mdo1char;
	    $line = <$handle>;              	# Read 'til first declaration
	    $this->find_ext_parm_ref(\$line, $include)
		if $include == $IncMS;		# Read any external files
	    last if eof($handle);           	# Exit if EOF

	    $c = getc($handle);
	    if ($c eq $mdo2char) { 		# Read declaration
		last DTDPARSELOOP  unless
		$this->read_declaration($handle, $include);

	    } elsif ($c eq $pio2char) {		# Read processing instruction
		$this->read_procinst($handle, $include);

	    } else {
		&errMsg("Error: Unrecognized markup: $line$c\n");
		die;
	    }
	}
      }
    }; # end eval

    select($old);				# Reset default filehandle
    $/ = $oldslash;				# Reset $/

    $@ ? 0 : 1;
}

##***************************************************************************##
##				CLASS METHODS				     ##
##***************************************************************************##

##---------------------------------------------------------------------------
##	set_comment_callback() sets the function to be called when an
##	SGML comment declaration is encountered.
##
sub set_comment_callback {
    my $class = shift;

    $CommentCallback = shift;
}

##---------------------------------------------------------------------------
##	set_verbosity() sets the verbosity flag.  Setting it to a
##	non-zero value cause read_dtd() to output status messages
##	as it parses a DTD.
##
sub set_verbosity {
    my $class = shift;
    $Verbose = shift;
}

##---------------------------------------------------------------------------
##	set_pi_callback() sets the function to be called when a
##	processing instruction is encountered.
##
sub set_pi_callback {
    my $class = shift;
    $PICallback = shift;
}

##---------------------------------------------------------------------------
##	set_tree_callback() sets the function to be called before
##	an entry is printed in the print_tree function.
##
sub set_tree_callback {
    my $class = shift;
    $PrTreeEntry = $_[0] || \&pr_tree_entry;
}

##---------------------------------------------------------------------------
##	set_debug_callback() sets the debug callback to call when
##	DTD.pm generates a debugging message.
##
sub set_debug_callback {
    my $class = shift;
    $DebugCallback = shift;
}

##---------------------------------------------------------------------------
##	set_debug_handle() sets the debug filehandle where all
##	debugging messages will go.
##
sub set_debug_handle {
    my $class = shift;
    $DebugHandle = shift;
}

##---------------------------------------------------------------------------
##	set_ent_manager() sets the entity manager to use for resolving
##	external entities.
##
sub set_ent_manager {
    my $class = shift;
    $EntMan = shift;
}

##---------------------------------------------------------------------------
##	set_err_callback() sets the error callback to call when
##	DTD.pm generates a error message.
##
sub set_err_callback {
    my $class = shift;
    $ErrMsgCallback = shift;
}

##---------------------------------------------------------------------------
##	set_err_handle() sets the error filehandle where all
##	error messages will go.
##
sub set_err_handle {
    my $class = shift;
    $ErrHandle = shift;
}


##***************************************************************************##
##			       PRIVATE METHODS				     ##
##***************************************************************************##
##	The following are methods that are not meant to be called
##	outside of this class/package.
##***************************************************************************##

##---------------------------------------------------------------------------
##	read_declaration() parses a declaration.  A return of 0 signifies
##	that parsing of DTD should terminate (ie. DOCTYPE declaration
##	parsed).
##
sub read_declaration {
    my $this = shift;

    my($handle, $include) = @_;
    my($d) = $/;
    my($c, $line, $func, $tmp, $i, $q);
    $line = '';

    $c = getc($handle);
    if ($c eq $comchar) {			# Comment declaration
	$this->read_comment($handle);
	return 1;
    }
    if ($c eq $dso_) {				# Marked section
	$this->read_msection($handle, $include);
	return 1;
    }

    $func = $c;
    while ($c !~ /^\s*$/) {     # Get declaration type
        $c = getc($handle);
        $func .= $c;
    }
    chop $func;
    $func =~ tr/a-z/A-Z/;	# Translate declaration type to uppercase

    if ($func =~ /^\s*$DOCTYPE\s*$/oi) {	# DOCTYPE declaration
	$this->read_doctype($handle, $include);
	return 0;
    }
    if ($func =~ /^\s*$LINKTYPE\s*$/oi) {	# LINKTYPE declaration
	$this->read_linktype($handle, $include);
	return 1;
    }

    while ($c ne $mdc) {		# Get rest of declaration
        $c = getc($handle);		    # Get next character
        if ($c eq $comchar) {		    # Check for comment
            $i = getc($handle);			# Get next character
            if ($i eq $comchar) { 	 	# Remove in-line comments
                $/ = $comc_;  $tmp = <$handle>; # Slurp comment
            } elsif ($i =~ /[$quotes]/o) {	# Check for quoted string
		$/ = $i;  $tmp = <$handle>;	# Slurp string
		$line .= $c . $i . $tmp;
	    } else {				# Save characters
		$line .= $c . $i;
		$c = $i;			# Set $c for while condition
	    }
        } elsif ($c =~ /[$quotes]/o) {	    # Check for quoted string
	    $/ = $c;  $tmp = <$handle>;
	    $line .= $c . $tmp;
	} else {			    # Save character
	    $line .= $c;
	}
    }
    if ($include == $IncMS) {		# Process declaration if including
	chop $line;			    # Remove close delimiter
	$line =~ s/\n/ /g;		    # Translate newlines to spaces
	$tmp = $Function{$func};
	&$tmp($this, \$line) if $tmp;	    # Interpret declaration
    }
    $/ = $d;				# Reset slurp var
    1;
}

##---------------------------------------------------------------------------
##	read_procinst() reads in a processing instruction.
##
sub read_procinst {
    my $this = shift;
    my($handle, $include) = @_;
    my($d) = $/;
    my($txt, $i);

    $/ = $pic_;			# Set slurp var to '>'
    $txt = <$handle>;		# Get pi text
    &debugMsg("Processing instruction: $id\n");
    if ($include == $IncMS) {
	if (defined(&$PICallback)) {	# Call pi callback if defined.
	    &debugMsg("\tInvoking $PICallback\n");

	    for ($i=0; $i < length($/); $i++) {
		chop $txt; }		# Remove close delimiter
	    &$PICallback(\$txt);
	}
    }
    $/ = $d;			# Reset slurp var
}

##---------------------------------------------------------------------------
##	read_comment() slurps up a comment declaration.
##
sub read_comment {
    my $this = shift;
    my($handle) = @_;
    my($d) = $/;
    my($txt, $i, $tmp);
    $txt = '';

    &debugMsg("Comment declaration\n");
    getc($handle);		# Read second comment character
    while (1) {			# Get comment text
	$/ = $mdc_;		    		# Set slurp var to ">"
	$tmp = <$handle>;
	$txt .= $tmp;
	last if $tmp =~ /$comc\s*$mdc$/o;	# Check for close
    }
    if (defined(&$CommentCallback)) {	# Call comment callback if defined.
	&debugMsg("\tInvoking $CommentCallback\n");

	$txt =~ s/^([\S\s]*)$comc\s*$mdc$/$1/o;	# Remove comment close
	$txt = ' ' x length($mdo_ . $como_) . $txt;
	&$CommentCallback(\$txt);
    }
    $/ = $d;			# Reset slurp var
}

##---------------------------------------------------------------------------
##	read_doctype() parses a DOCTYPE declaration.
##
sub read_doctype {
    my $this = shift;
    my($handle, $include) = @_;
    my($line, $dt, $tok, $tok2, $extsubhandle);
    my($extsubpubid, $extsubsysid) = ('', '');
    my($d) = $/;

    ##	Should be processing one DOCTYPE at most.
    if ($this->{DocType} && $include) {
	&errMsg("Warning: Extra DOCTYPE declaration ignored\n");
    }

    ##	Get text before DSO
    $line = '';
    $/ = $dso_;
    while (!eof($handle)) {
	$line .= <$handle>;
	last if &notin_lit($line);
    }
    $line =~ s/${dso}$//o;		# Strip DSO
    &debugMsg("$DOCTYPE $line\n");

    ##  Get doctype name
    if ($include) {
	$dt = &get_next_group(\$line);
	($this->{DocType} = $dt) =~ tr/a-z/A-Z/  unless $this->{DocType};

	##  Check for external identifier
	if ($tok = &get_next_group(\$line)) {
	    if ($tok =~ /$PUBLIC/o) {
		$extsubpubid = &get_next_group(\$line);
	    }
	    $extsubsysid = &get_next_group(\$line);
	}
    }

    ##	Read local subset
    $this->read_subset($handle, $include, $dsc_.$mdc_);

    ##	Read external subset
    if ($include && ($extsubpubid || $extsubsysid)) {
	my $dtent = $EntMan->open_doctype($this->{DocType},
					  $extsubpubid, $extsubsysid);
	if ($dtent) {
	    &debugMsg("Reading $DOCTYPE external subset\n");
	    $this->read_dtd($dtent, $include);
	    close($dtent);
	} else {
	    errMsg("Warning: Unable to access $DOCTYPE external subset\n");
	}
    }

    &debugMsg("Finished $DOCTYPE\n");
    $/ = $d;				# Reset slurp var
}

##---------------------------------------------------------------------------
##	read_linktype() parses a LINKTYPE declaration.  $include determines
##	if the declaration is to be included or ignored.
##
sub read_linktype {
    my $this = shift;

    my($handle, $include) = @_;
    my($line);
    my($d) = $/;

    $/ = $dso_;
    $line = <$handle>;                  # Get text before $dso
    $this->expand_entities(\$line);
    &errMsg("Warning: $LINKTYPE declaration ignored\n");
    $this->read_subset($handle, $IgnMS, $dsc_.$mdc_);
    $/ = $d;				# Reset slurp var
}

##---------------------------------------------------------------------------
##	read_msection() parses marked section.  $include determines
##	if the section is to be included or ignored.
##
sub read_msection {
    my $this = shift;
    my($handle, $include) = @_;
    my($line);
    my($d) = $/;

    $/ = $dso_;
    $line = <$handle>;                  # Get status keyword
    $this->expand_entities(\$line);
    &debugMsg("Begin Marked Section: $line\n");

    if ($line =~ /$RCDATA/io || $line =~ /$CDATA/io) {	# Ignore (R)CDATA
	$this->slurp_msection($handle);

    } elsif ($line =~ /$IGNORE/io) {			# Check for IGNORE
	$this->ignore_msection($handle);

	# $include = $IgnMS;
	# $this->read_subset($handle, $include, $msc_.$mdc_);

    } else {
	$this->read_subset($handle, $include, $msc_.$mdc_);
    }

    &debugMsg("End Marked Section\n");
    $/ = $d;				# Reset slurp var
}

##---------------------------------------------------------------------------
##	slurp_msection() skips past a marked section that cannot include
##	nested marked sections.  This routine is used when RCDATA or
##	CDATA marked sections are encountered.
##
sub slurp_msection {
    my $this = shift;
    my($handle) = @_;
    my($d) = $/;
    $/ = "${msc_}${mdc_}";
    <$handle>;
    $/ = $d;				# Reset slurp var
}

##---------------------------------------------------------------------------
##	ignore_msection() skips past an ignore marked section.  A
##	check is made for nested marked sections to properly terminate
##	the ignored section.
##
sub ignore_msection {
    my $this = shift;
    my($handle) = @_;
    my($d) = $/;
    my($opencnt) = (1);		# Initial open already read
    my($igtxt) = ('');

    while (($opencnt > 0) && !eof($handle)) {
	$/ = "${msc_}${mdc_}";
	$igtxt = <$handle>;
	$opencnt += ($igtxt =~ s/${mdo}${dso}//go);
	$opencnt--;
    }

    $/ = $d;				# Reset slurp var
}

##---------------------------------------------------------------------------
##	read_subset() parses a subset section.  $include determines
##	if the subset is included or ignored.  $endseq signifies the
##	end delimiting sequence of the subset.
##
sub read_subset {
    my $this = shift;
    my($handle, $include, $endseq) = @_;
    my($c, $i, $line);
    my(@chars) = split(//, $endseq);

    &debugMsg("Begin Subset\n");
    while (1) {
        $c = getc($handle);  next if $c =~ /^\s$/;
        if ($c eq $mdo1char) {     	# declaration statement
            $c = getc($handle);
	    if ($c eq $mdo2char) {		# Read declaration
		$this->read_declaration($handle, $include);

	    } elsif ($c eq $pio2char) {		# Read processing inst.
		$this->read_procinst($handle, $include);

	    } else {				# Invalid character
		&subset_error($c, "Invalid second character for MDO or PIO");
	    }
        }
        elsif ($c eq $chars[0]) {		# End of subset section
	    for ($i=1; $i <= $#chars; ) {
		$c = getc($handle);
		if ($c eq $chars[$i]) { $i++; }		# Part of $endseq
		elsif ($c =~ /^\s$/) { next; }		# Whitespace
		else { last; }
	    }
	    if ($i > $#chars) {
		&debugMsg("End Subset\n");
		return;
	    }
        }
        elsif ($c eq $pero) {			# Ext parm entity ref
            $line = $c;
            while (1) {
                $c = getc($handle);
                if ($c =~ /[$namechars]/o) { $line .= $c; }
                else { last; }
            }
            $this->find_ext_parm_ref(\$line, $include) if $include == $IncMS;
        }
        else {
	    &subset_error($c,
		"Invalid character found outside of a markup statment");
        }
    }
}

##---------------------------------------------------------------------------
##	find_ext_parm_ref() evaulates in external parameter entity
##	references in \$line.  $include is the INCLUDE/IGNORE flag
##	that is passed to read_dtd.
##
sub find_ext_parm_ref {
    my $this = shift;
    my($line, $include) = @_;
    my($i, $tmp);
    while ($$line =~ /$pero/o) {
        $$line =~ s/$pero([$namechars]+)$refc?//o;
        if (defined($i = $this->resolve_ext_entity_ref($1))) {
	    $this->read_dtd($i, $include);
	    close($i);
        }
    }
}

##---------------------------------------------------------------------------
##	do_attlist() process an attribute list declaration.
##
sub do_attlist {
    my $this = shift;
    my($line) = @_;
    my($tmp, $attname, $attvals, $attdef, $fixval, $attr,
	  @array, $notation);

    $attr = { };	# Create has to attribute values

    $this->expand_entities($line);
    $tmp = &get_next_group($line);	 	# Get element name(s)
    if ($tmp =~ /^\s*$rni$NOTATION\s*$/io) {	# Check for #NOTATION
	&errMsg("Warning: $ATTLIST $rni$NOTATION skipped\n");
	return;
    }
    &debugMsg("$ATTLIST: $tmp\n");
    $tmp =~ s/($grpo|$grpc|\s+)//go;
    $tmp =~ tr/A-Z/a-z/;		 # Convert all names to lowercase
    @names = split(/[$or$and$seq\s]+/o, $tmp);
    while ($$line !~ /^\s*$/) {
	$attname = &get_next_group($line);
	$attname =~ tr/A-Z/a-z/;	 # Convert attribute name to lowercase
	$attvals = &get_next_group($line);
	if ($attvals =~ /^\s*$NOTATION\s*$/io) {	# Check for NOTATION
	    $notation = 1;
	    $attvals = &get_next_group($line);
	} else {
	    $notation = 0;
	}
	$attdef  = &get_next_group($line);
	if ($attdef =~ /^\s*$rni$FIXED\s*$/io) {	# Check for #FIXED
	    $fixval = &get_next_group($line);
	} else {
	    $fixval = "";
	}
	$attvals =~ s/[$grpo$grpc\s]//go;
	@array = split(/[$seq$and$or]/o, $attvals);
	unshift(@array, $NOTATION) if $notation;
	if ($fixval) {
	    $attr->{$attname} = [$attdef, $fixval, @array];
	} else {
	    $attr->{$attname} = [$attdef, @array];
	}
    }

    ##	Store attribute information for each element
    foreach (@names) {
	$this->{Attribute}{$_} = $attr;
    }

    ##	Create mapping of attribute name to element
    foreach (keys %$attr) {
	push(@{$this->{ElemsOfAttr}{$_}}, @names);
    }
}

##---------------------------------------------------------------------------
##	do_element processes an element declaration.
##
sub do_element {
    my $this = shift;
    my($line) = @_;
    my($tmp, @names, $tagm, $elcont, $elinc, $elexc);
    $elinc = '';  $elexc = '';

    $this->expand_entities($line);
    $tmp = &get_next_group($line);	 # Get element name(s)
    &debugMsg("$ELEMENT: $tmp\n");
    $tmp =~ s/[$grpo$grpc\s]//go;
    $tmp =~ tr/A-Z/a-z/;		 # Convert all names to lowercase
    @names = split(/[$or$and$seq\s]+/o, $tmp);

    if ($$line =~ s/^([-Oo]{1})\s+([-Oo]{1})\s+//) { # Get tag minimization
	($tagm = "$1 $2") =~ tr/o/O/;
    } else {
	$tagm = "- -";
    }
 
    $elcont = &get_next_group($line);	 # Get content

    if ($elcont ne $EMPTY) {		 # Get inclusion/exclusion groups
	$elcont =~ tr/A-Z/a-z/;
	while ($$line !~ /^\s*$/) {
	    if ($$line =~ /^$inc/o) { $elinc = &get_inc($line); }
	    elsif ($$line =~ /^$exc/o) { $elexc = &get_exc($line); }
	    else { last; }
	}
	$elinc =~ tr/A-Z/a-z/;
	$elexc =~ tr/A-Z/a-z/;
    }

    foreach (@names) {			# Store element information
	if (defined($this->{ElemCont}{$_})) {
	    &errMsg("Warning: Duplicate element declaration: $_\n");
	} else {
	    $this->{ElemCont}{$_} = $elcont;
	    $this->{ElemInc}{$_} = $elinc;
	    $this->{ElemExc}{$_} = $elexc;
	    $this->{ElemTag}{$_} = $tagm;
	    push(@{$this->{Elements}}, $_);
	}
    }
}

##---------------------------------------------------------------------------
##	do_entity process an entity declaration
##
sub do_entity {
    my $this = shift;
    my($line) = @_;

    &debugMsg("Entity Declaration\n\t", $$line, "\n");
    if ($$line =~ /^\s*$pero/o) { $this->do_parm_entity($line); }
    else { $this->do_gen_entity($line); }
}

##---------------------------------------------------------------------------
##	do_notation processes a notation declaration
##
sub do_notation {
    my $this = shift;
    my($line) = @_;
    my($name);

    $name = &get_next_group($line);
    &debugMsg("$NOTATION $name\n");

    if ($$line =~ s/^$SYSTEM\s+//io) {		# SYSTEM notation
	$this->{SysNotation}{$name} = &get_next_group($line)
	    unless defined($this->{SysNotation}{$name});

    } else {				  	# PUBLIC notation
	$$line =~ s/^$PUBLIC\s+//io;
	$this->{PubNotation}{$name} = &get_next_group($line)
	    unless defined($this->{PubNotation}{$name});
    }
}

##---------------------------------------------------------------------------
##	do_shortref processes a shortref declaration.
##
sub do_shortref {
    my $this = shift;
    &errMsg("Warning: $SHORTREF declaration ignored\n");
}

##---------------------------------------------------------------------------
##	do_usemap processes a usemap declaration.
##
sub do_usemap {
    my $this = shift;
    &errMsg("Warning: $USEMAP declaration ignored\n");
}

##---------------------------------------------------------------------------
##	expand_entities() expands all entity references in \$line.
##
sub expand_entities {
    my $this = shift;
    my($line) = @_;

    while ($$line =~ /($pero|$ero|$cro)[$namechars]+$refc?/o) {
	$this->expand_parm_entities($line);
	$this->expand_gen_entities($line);
	&expand_char_entities($line);
    };
}

##---------------------------------------------------------------------------
##	expand_parm_entities() expands all parameter entity references
##	in \$line.
##
sub expand_parm_entities {
    my $this = shift;
    my($line) = @_;

    while ($$line =~ s/$pero([$namechars]+)$refc?/$this->{ParEntity}{$1}/) {
	&errMsg(qq|Warning: Parameter entity "$1" not defined.  |,
	        qq|May cause parsing errors.\n|)
	    unless defined($this->{ParEntity}{$1});
	&del_comments($line);
    }
}

##---------------------------------------------------------------------------
##	expand_gen_entities() expands all general entity references
##	in \$line.
##
sub expand_gen_entities {
    my $this = shift;
    my($line) = @_;

    while ($$line =~ s/$ero([$namechars]+)$refc?/$this->{_AGE}{$1}/) {
	&errMsg(qq|Warning: Entity "$1" not defined.  |,
		qq|May cause parsing errors.\n|)
	    unless defined($this->{_AGE}{$1});
	&del_comments($line);
    }
}

##---------------------------------------------------------------------------
##	resolve_ext_entity_ref() translates an external entity to
##	its corresponding filename.  The entity identifier is checked
##	first.  If that fails, then the entity name
##	itself is used for resolution.
##
sub resolve_ext_entity_ref {
    my $this = shift;
    my($ent, $pubid, $sysid) = @_;
    my $fh = undef;

    $pubid = $this->{PubParEntity}{$ent} unless $pubid;
    $sysid = $this->{SysParEntity}{$ent} unless $sysid;

    BLK: {
	if (not $pubid || $sysid) {
	    &errMsg("Warning: Entity referenced, but not defined: $ent\n"),
	    last BLK;
	}
	if ($EntMan) {
	    $fh = $EntMan->open_entity("%ent", $pubid, $sysid);
	    last BLK;
	}
	&errMsg("Warning: Unable to resolve entity reference: $ent\n");
    }

    $fh;
}

##---------------------------------------------------------------------------
##	do_parm_entity() parses a parameter entity definition.
##
sub do_parm_entity {
    my $this = shift;
    my($line) = @_;
    my($name, $value);

    $$line =~ s/^\s*$pero?\s+//o;	  # Remove pero, '%'
    $$line =~ s/^(\S+)\s+//; $name = $1;   # Get entity name

    if ($$line =~ s/^$PUBLIC\s+//io) {	  	# PUBLIC external parm entity
	$this->{PubParEntity}{$name} = &get_next_group($line)
	    unless defined($this->{PubParEntity}{$name});

    } elsif ($$line =~ s/^$SYSTEM\s+//io) {	# SYSTEM external parm entity
	$this->{SysParEntity}{$name} = &get_next_group($line)
	    unless defined($this->{SysParEntity}{$name});

    } else {				  	# Regular parm entity
	if (!defined($this->{ParEntity}{$name})) {
	    $value = &get_next_group($line);
	    &del_comments(\$value);
	    $this->{ParEntity}{$name} = $value;
	    push(@{$this->{ParEntities}}, $name);
	}
    }
}

##---------------------------------------------------------------------------
##	do_gen_entity() parses a general entity definition.
##
sub do_gen_entity {
    my $this = shift;
    my($line) = @_;
    my($name, $tmp);

    $$line =~ s/^\s*(\S+)\s+//; $name = $1;   # Get entity name
    &debugMsg("$ENTITY $name\n");
    $tmp = &get_next_group($line);
    GENSW: {
	$this->do_ge_starttag($name, $line), last GENSW
	    if $tmp =~ /^\s*$STARTTAG\s*$/io;
	$this->do_ge_endtag($name, $line), last GENSW
	    if $tmp =~ /^\s*$ENDTAG\s*$/io;
	$this->do_ge_ms($name, $line), last GENSW
	    if $tmp =~ /^\s*$MS\s*$/io;
	$this->do_ge_md($name, $line), last GENSW
	    if $tmp =~ /^\s*$MD\s*$/io;
	$this->do_ge_pi($name, $line), last GENSW
	    if $tmp =~ /^\s*$PI\s*$/io;
	$this->do_ge_cdata($name, $line), last GENSW
	    if $tmp =~ /^\s*$CDATA\s*$/io;
	$this->do_ge_sdata($name, $line), last GENSW
	    if $tmp =~ /^\s*$SDATA\s*$/io;
	$this->do_ge_public($name, $line), last GENSW
	    if $tmp =~ /^\s*$PUBLIC\s*$/io;
	$this->do_ge_system($name, $line), last GENSW
	    if $tmp =~ /^\s*$SYSTEM\s*$/io;
	$this->{_AGE}{$name} = $this->{GenEntity}{$name} = $tmp;
    }
    push(@{$this->{GenEntities}}, $name);
}

##---------------------------------------------------------------------------
sub do_ge_starttag {
    my $this = shift;
    my($name, $line) = @_;
    my($tmp);

    $tmp = &get_next_group($line);
    $this->{StartTagEntity}{$name} = $tmp;
}

sub do_ge_endtag {
    my $this = shift;
    my($name, $line) = @_;
    my($tmp);

    $tmp = &get_next_group($line);
    $this->{EndTagEntity}{$name} = $tmp;
}

sub do_ge_ms {
    my $this = shift;
    my($name, $line) = @_;
    my($tmp);

    $tmp = &get_next_group($line);
    $this->{MSEntity}{$name} = $tmp;
    $this->{_AGE}{$name} = $mdo_ . $dso_ . $tmp . $msc_ . $mdc_;
}

sub do_ge_md {
    my $this = shift;
    my($name, $line) = @_;
    my($tmp);

    $tmp = &get_next_group($line);
    $this->{MDEntity}{$name} = $tmp;
    $this->{_AGE}{$name} = $mdo_ . $tmp . $mdc_;
}

sub do_ge_pi {
    my $this = shift;
    my($name, $line) = @_;
    my($tmp);

    $tmp = &get_next_group($line);
    $this->{PIEntity}{$name} = $tmp;
    $this->{_AGE}{$name} = $pio_ . $tmp . $pic_;
}

sub do_ge_cdata {
    my $this = shift;
    my($name, $line) = @_;
    my($tmp);

    $tmp = &get_next_group($line);
    $this->{CDataEntity}{$name} = $tmp;
}

sub do_ge_sdata {
    my $this = shift;
    my($name, $line) = @_;
    my($tmp);

    $tmp = &get_next_group($line);
    $this->{SDataEntity}{$name} = $tmp;
}

sub do_ge_public {
    my $this = shift;
    my($name, $line) = @_;
    &errMsg("Warning: General $PUBLIC entity skipped\n");
}

sub do_ge_system {
    my $this = shift;
    my($name, $line) = @_;
    &errMsg("Warning: General $SYSTEM entity skipped\n");
}

##---------------------------------------------------------------------------
##	compute_parents() generates the %Parents and %TopElement arrays.
##
sub compute_parents {
    my $this = shift;

    return  if $this->{_DidParents};

    my($elem, %exc, @array);

    foreach $elem ($this->get_elements()) {
        foreach (&extract_elem_names($this->{ElemExc}{$elem})) {
	    $exc{$_} = 1;
	}

	@array = (&extract_elem_names($this->{ElemCont}{$elem}),
		  &extract_elem_names($this->{ElemInc}{$elem}));
	&remove_dups(\@array);

	foreach (@array) {
	    push(@{$this->{Parents}{$_}}, $elem)
		unless $exc{$_} || !$this->is_element($_);
	}
        %exc = ();
    }
    foreach (keys %{$this->{ElemCont}}) {
	$this->{TopElement}{$_} = 1
	    if !$this->{Parents}{$_} or $this->{Parents}{$_} eq $_;
    }
    $this->{_DidParents} = 1;
}

##---------------------------------------------------------------------------
##	compute_levels() is the first pass over the element content
##	hierarchy.  It determines the highest level each element occurs
##	in the DTD.
##
sub compute_levels {
    my $this = shift;
    my($elem, $level) = @_;
    my(@array, @incarray, @excarray, %notdone, %lexc);

    return if $level > $MaxLevel;

    $done{$elem} = $level if ($level < $done{$elem} || !$done{$_});

    ## Get inclusion elements ##
    @incarray = sort &extract_elem_names($this->{ElemInc}{$elem});
    foreach (@incarray) { $inc{$_}++; }

    ## Get element contents ##
    @array = (@incarray, &extract_elem_names($this->{ElemCont}{$elem}));
    &remove_dups(\@array);
    foreach (@array) {
	next unless $this->is_element($_);

        $done->{$_} = $level+1, $notdone{$_} = 1
            if ($level+1 < $done{$_} || !$done{$_});
    }

    ## Get exclusion elements ##
    @excarray = sort &extract_elem_names($this->{ElemExc}{$elem});
    foreach (@excarray) { $exc{$_}++; $lexc{$_} = 1; }

    ## Compute sub tree ##
    foreach (@array) {
	next unless $this->is_element($_);

        if (!$lexc{$_}) {
            $this->compute_levels($_, $level+1),
                $notdone{$_} = 0  if ($level < $MaxLevel &&
                                      ($level+1 < $done{$_} || $notdone{$_}));
        }
    }
    ## Remove include elements ##
    foreach (@incarray) { $inc{$_}--; }
    ## Remove exclude elements ##
    foreach (@excarray) { $exc{$_}--; }
}

##---------------------------------------------------------------------------
##	print_sub_tree() is the second pass of an element content
##	hierarchy.  It actually prints the tree, and it uses the
##	%done array built by compute_levels() to perform pruning.
##
sub print_sub_tree {
    my $this = shift;
    my($elem, $level) = @_;
    my(%lexc, %linc, %pad, %elem2pr);
    my(@array, @incarray, @excarray, @aincarray, @aexcarray);
    my($tmp, $i, $item, $curelem, $prtxt, $hascontent, $key, $o);

    return if $level > $MaxLevel;
    $done{$elem} = 0;	# Set done value so $elem tree is printed only once.
    $key = 0;		# Key counter for mapping elements to printed
			# element.  The gi cannot be used since a content
			# model may contain duplicate elements.

    ##	Get element contents
    ##	    This block grabs the content model of the element and
    ##	    creates a mapping of subelements to the printed copy.
    ##	    Delimiters are preserved and indenting is done for
    ##	    model groups.
    ##
    @array = &extract_elem_names($this->{ElemCont}{$elem},1);
    $hascontent = (scalar(@array) != 1);
    if (scalar(@array) == 1) {
	($tmp = $array[0]) =~ tr/a-z/A-Z/;
	$elem2pr{$key++} = $tmp;
    } else {
	$curelem = ''; $o = 0; $prtxt = '';
	foreach $item (@array) {
	    if ($item eq $grpo_) {
		if ($curelem) {
		    $elem2pr{$tmp} = $prtxt;
		    $curelem = '';
		    $prtxt = ('_' x $o) . $item;
		} else {
		    $prtxt .= $item;
		}
		$o++;
		next;
	    }
	    if ($item eq $grpc_) {
		$o--;
		$prtxt .= $item;
		next;
	    }
	    if ($item eq $and_ || $item eq $or_ || $item eq $seq_) {
		$prtxt .= " "  unless $item eq $seq_;
		$prtxt .= $item;
		$elem2pr{$tmp} = $prtxt;
		$curelem = '';
		$prtxt = '_' x $o;
		next;
	    }
	    if ($item eq $opt_ || $item eq $plus_ || $item eq $rep_) {
		$prtxt .= $item;
		next;
	    }
	    $curelem = $item;
	    $tmp = $key++;
	    $pad{$tmp} = $o;		# Track padding for group indentation
	    $item =~ tr/a-z/A-Z/
		if ($item =~ /$rni/o) || !$this->is_element($curelem);
	    $prtxt .= $item;
	}
	$elem2pr{$tmp} = $prtxt;
    }

    ## List inclusion elements due to ancestors ##
    @aincarray = sort grep($inc{$_} > 0, sort keys %$inc);
    if (scalar(@aincarray) && $hascontent) {
        $tmp = '{A+}';
        foreach (@aincarray) { $tmp .= ' ' . $_; }
        &print_elem($tmp, 0, $level);
    }

    ## List exclusion elements due to ancestors ##
    @aexcarray = sort grep($exc{$_} > 0, sort keys %$exc);
    if (scalar(@aexcarray) && $hascontent) {
        $tmp = '{A-}';
        foreach (@aexcarray) { $tmp .= ' ' . $_; }
        &print_elem($tmp, 0, $level);
    }

    ## Get inclusion elements ##
    @incarray = sort &extract_elem_names($this->{ElemInc}{$elem});
    if (scalar(@incarray)) {
	$tmp = ' {+}';
	foreach (@incarray) {
	    $inc{$_}++;
	    $linc{$_} = 1;
	    $tmp .= ' ' . $_;
	    $elem2pr{$key++} = $_;
	}
	&print_elem($tmp, 0, $level);
    }

    ## Get exclusion elements ##
    @excarray = sort &extract_elem_names($this->{ElemExc}{$elem});
    if (scalar(@excarray)) {
	$tmp = ' {-}';
	foreach (@excarray) {
	    $exc{$_}++;
	    $lexc{$_} = 1;
	    $tmp .= ' ' . $_;
	}
	&print_elem($tmp, 0, $level);
    }
    &print_elem('', 1, $level)
	if $hascontent &&
	   (scalar(@excarray) || scalar(@incarray) ||
	    scalar(@aincarray) || scalar(@aexcarray));

    ## Output sub trees ##
    my($prefix, $suffix);
    @array = (&extract_elem_names($this->{ElemCont}{$elem}), @incarray);
    $i = 0;
    foreach (@array) {
	$open{$level} = ($i < $#array ? 1 : 0);
	$prefix = ''; $suffix = '';
	if ($this->is_element($_)) {
	    if ($lexc{$_}) {
		$suffix .= " {-}";
	    } elsif ($linc{$_}) {
		$suffix .= " {+}";
	    }

	    if (!$lexc{$_} && ($done{$_} < $level)) {
		$suffix .= " ...";
	    }
	}
	&print_elem($prefix . $elem2pr{$i} . $suffix, 1, $level);

	push(@padlen, $pad{$i});
        if ($this->is_element($_) && !$lexc{$_}) {
            $this->print_sub_tree($_, $level+1)
                if ($level < $MaxLevel && $level == $done{$_});
        }
	pop(@padlen);

    } continue {
	$i++;
    }

    &print_elem("", 0, $level);

    ## Remove include elements ##
    foreach (@incarray) { $inc{$_}--; }
    ## Remove exclude elements ##
    foreach (@excarray) { $exc{$_}--; }
}

##***************************************************************************##
##			    PRIVATE FUNCTIONS				     ##
##***************************************************************************##
##	These routines are not meant to be called by outside of this
##	package.  However, there may be exceptions.
##***************************************************************************##

##---------------------------------------------------------------------------
##	debugMsg() either calls registered error message callback or
##	prints list to error filehandle when verbosity is set.
##
sub debugMsg {
    if ($Verbose) {
	my(@dlist) = ("Debug: ", @_);
	if (defined(&$DebugCallback)) {
	    &$DebugCallback(@dlist);
	} else {
	    print($DebugHandle  @dlist);
	}
    }
}

##---------------------------------------------------------------------------
##	errMsg() either calls registered error message callback, or
##	prints list to error filehandle.
##
sub errMsg {
    if (defined(&$ErrMsgCallback)) {
	&$ErrMsgCallback(@_);
    } else {
	print($ErrHandle  @_);
    }
}

##----------------------------------------------------------------------
##      notin_lit() checks if string has a literal that is open.
##      The function returns 1 if it is not. Else it returns 0.
##
sub notin_lit {
    my($str) = ($_[0]);
    my($q, $after);
 
    while ($str =~ /([${lit}${lita}])/o) {
        $q = $1;
        $after = $';
        if (($q eq $lit ? ($after =~ /($lit)/o) :
                          ($after =~ /($lita)/o)) ) {
            $str = $';
        } else {
            return 0;
        }
    }
    1;
}

##---------------------------------------------------------------------------
##      zip_wspace() takes a ref to a string and strips all beginning
##      and ending whitespaces.  It also compresses all other whitespaces
##      into a single space character.
##
sub zip_wspace {
    local($str) = shift;
    $$str =~ s/^\s*(.*[^\s])\s*$/$1/;
    $$str =~ s/\s{2,}/ /g;
}

##---------------------------------------------------------------------------
##      quote_chars() escapes special characters in case passed in string
##      will get be used in a pattern matching statement.  This prevents
##      the string from causing perl to barf because the string happens
##      to contain characters that have special meaning in pattern
##      matches.
##
##	Passed in string is by reference.
##
sub quote_chars {
    my($str) = @_;
    $$str =~ s/(\W)/\\$1/g;
}

##---------------------------------------------------------------------------
sub unquote_chars {
    my($str) = @_;
    $$str =~ s/\\//g;
}

##---------------------------------------------------------------------------
##	extract_elem_names() extracts just the element names of $str.
##	An array is returned.  The elements in $str are assumed to be
##	separated by connectors.
##
##	The $andcon is flag if the connector characters are included
##	in the array.
##
sub extract_elem_names {
    my($str, $andcon) = @_;
    my(@ret_a);
    if ($andcon) {
	my($exchar) = ('');
	$str =~ s/\s//go;
	if ($str =~ s/^([$inc$exc])//o)	# Check for exception rules
	    { $exchar = $1; }
	@ret_a = ($exchar,
	          split(/([$seq$and$or$grpo$grpc$opt$plus$rep])/o, $str));
    }
    else {
	$str =~ s/^\s*[$inc$exc]//o;	# Check for exception rules
	$str =~ s/[$grpo$grpc$opt$plus$rep\s]//go;
	@ret_a = (split(/[$seq$and$or]/o, $str));
    }
    grep($_ ne '', @ret_a);		# Strip out null items
}

##---------------------------------------------------------------------------
##	get_inc() gets the inclusion element group of an element
##	definition from \$line.
##
sub get_inc {
    my($line) = @_;
    my($ret);
    $$line =~ s/^$inc\s*//o;
    $ret = &get_next_group($line);
    $ret;
}

##---------------------------------------------------------------------------
##	get_exc() gets the exclusion element group of an element
##	definition from \$line.
##
sub get_exc {
    my($line) = @_;
    my($ret);
    $$line =~ s/^$exc\s*//o;
    $ret = &get_next_group($line);
    $ret;
}

##---------------------------------------------------------------------------
##	get_next_group gets the next group from a declaration in \$line.
##
sub get_next_group {
    my($line) = @_;
    my($o, $c, $tmp, $ret);
    $ret = '';

    $$line =~ s/^\s*//;
    $c = 0;
    if ($$line =~ /^$grpo/o) {
	$o = 1;
	while ($o > $c) {
	    $$line =~ s/^([^$grpc]*${grpc}[${opt}${plus}${rep}]?)//o;
	    $ret .= $1;
	    $tmp = $ret;
	    $o = $tmp =~ s/$grpo//go;
	    $c = $tmp =~ s/$grpc//go;
	}
	$$line =~ s/^\s*//;

    } elsif ($$line =~ /^[$quotes]/o) {
	$ret = &get_next_string($line);

    } elsif ($$line =~ /\S/) {
	$$line =~ s/^(\S+)\s*//;
	$ret = $1;
    }

    &zip_wspace(\$ret);
    $ret;
}

##---------------------------------------------------------------------------
##	get_next_string() gets the next literal from a string.  This
##	function is used by the do*entity routines.
##
sub get_next_string {
    my($line) = @_;
    my($ret, $q);

    $$line =~ s/^\s*([$quotes])//o;  $q = $1;
    if ($q eq $lit_) {
	$$line =~ s/^([^$lit]*)$lit\s*//o;  $ret = $1;
    } else {
	$$line =~ s/^([^$lita]*)$lita\s*//o;  $ret = $1;
    }
    &zip_wspace(\$ret);
    $ret;
}

##---------------------------------------------------------------------------
##	is_quote_char() checks to see if $char is a quote character.
##
sub is_quote_char {
    $_[0] =~ /[$quotes]/o;
}

##---------------------------------------------------------------------------
##	remove_dups() removes duplicate items from an array
##
sub remove_dups {
    my($aref) = shift;
    my(%dup) = ();
    @$aref = grep($dup{$_}++ < 1, @$aref);
}

##---------------------------------------------------------------------------
##	subset_error() prints out a terse error message and dies.  This
##	routine is called if there is a syntax error in a subset section.
##
##	Print of character inside quotes, followed by the ASCII code for
##	easy identification, suggested by schampeo@aisg.com (06/01/94).
##
sub subset_error {
    my($c, $hint) = @_;
    &errMsg("Error: Syntax error in subset.\n",
	    qq|\tUnexpected character: "$c", ascii code=|, ord($c), ".\n",
	    ($hint ? "    Reason:\n\t$hint\n" : "\n"));
    die;
}

##---------------------------------------------------------------------------
##      del_comments() removes any inline comments from $line.
##      Unfortuneatly, this routines needs knowledge of the comment
##      delimiters.  If the deliminters are changed, this routine
##      must be updated.
##
sub del_comments {
    my($line) = @_;
    $$line =~ s/$como([^-]|-[^-])*$comc//go;
}

##---------------------------------------------------------------------------
##	expand_char_entities() expands all character entity references
##	in string referenced by $line.
##
sub expand_char_entities {
    my($line) = @_;

    while ($$line =~ s/$cro([$namechars]+)$refc?/$CharEntity{$1}/) {
	&errMsg(qq|Warning: Character entity "$1" not recognized.  |,
	        qq|May cause parsing errors.\n|)
	    unless defined($CharEntity{$1});
    }
}

##---------------------------------------------------------------------------
##	print_elem() is used by print_sub_tree() to output the elements
##	in a structured format to $TreeFile.
##
sub print_elem {
    my($elem, $iselem, $level) = @_;
    my($i, $indent);

    if ($level == 1) {
	print $TreeFile sprintf("%s", &$PrTreeEntry($iselem, "$elem\n"));
    } else {
	$indent .= " " x $padlen[0];
	for ($i=2; $i < $level; $i++) {
	    $indent .= $open{$i} ? " | " : "   ";
	    $indent .= " " x $padlen[$i-1];
	}
	if ($iselem) {
	    $indent .= $elem ? " |_" : " | "; 
	} elsif ($elem ne "") {
	    $indent .= " | "; 
	}
	print $TreeFile
	      sprintf("%s", &$PrTreeEntry($iselem, "$indent$elem\n"));
    }
}

##---------------------------------------------------------------------------
##	pr_tree_entry() is default print tree entry function.
##
sub pr_tree_entry {
    shift;
    @_;
}

##---------------------------------------------------------------------------##
1;

