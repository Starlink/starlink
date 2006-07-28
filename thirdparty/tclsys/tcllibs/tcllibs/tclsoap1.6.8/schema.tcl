# schema.tcl - Copyright (C) 2002 Pat Thoyts <patthoyts@users.sf.net>
#
# Process XML Schema documents for WSDL types
# http://www.w3.org/TR/2001/REC-xmlschema-1-20010502/
#
#
#
# See:
# http://www.ruby-lang.org/cgi-bin/cvsweb.cgi/lib/soap4r/lib/wsdl/xmlSchema/
#
# We need to be able to fix the namespace name for xsi builtin types.
#
# -------------------------------------------------------------------------
# This software is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the accompanying file `LICENSE'
# for more details.
# -------------------------------------------------------------------------

package require log;                    # tcllib 1.0
package require uri;                    # tcllib 1.0
package require SOAP::Utils;            # TclSOAP

namespace eval ::SOAP::Schema {
    variable version 0.1
    variable rcsid {$Id$}

    catch {namespace import -force [namespace parent]::Utils::*}
}

# -------------------------------------------------------------------------

# called with the <schema> DOM element
proc ::SOAP::Schema::parse {schemaNode} {
    set defs {}
    foreach typeNode [getElements $schemaNode] {
        # now shift to XML schema parser...
        switch -exact -- [set def [nodeName $typeNode]] {
            complexType { lappend defs [parse_complexType $typeNode] }
            simpleType { lappend defs [parse_simpleType $typeNode] }
            import  {
                log::log warning "not done"
                #namespace=urn schemaLocation=url
            }
            default {
                log::log warning "unrecognised schema type:\
                        \"$def\" not handled"
            }
        }
    }
    return $defs
}

# http://www.w3.org/TR/2001/REC-xmlschema-1-20010502/#Simple_Type_Definitions
proc SOAP::Schema::parse_simpleType {typeNode} {
    set typeName [getElementAttribute $typeNode name]
    log::log debug "simpleType $typeName"
    foreach node [getElements $typeNode] {
        switch -exact -- [set style [getElementName $node]] {
            restriction -
            list -
            union {
                log::log debug "$typeName -> $style"
            }
        }
    }
}

# http://www.w3.org/TR/2001/REC-xmlschema-1-20010502/#Complex_Type_Definitions
proc SOAP::Schema::parse_complexType {typeNode} {
    set typeName [getElementAttribute $typeNode name]
    set typeNamespace [targetNamespaceURI $typeNode $typeName]

    set types {}
    foreach contentNode [getElements $typeNode] {
        lappend types [parse_content $contentNode]
    }
    return [list $types $typeNamespace:$typeName]
}

proc SOAP::Schema::parse_content {contentNode} {
    set contentName [nodeName $contentNode]
    switch -exact -- $contentName {
        complexContent -
        all {
            set r {}
            foreach node [getElements $contentNode] {
                set r [concat $r [parse_content $node]]
            }
            return $r
        }
        restriction {
            set base [getElementAttribute $contentNode base]
            set base [::SOAP::WSDL::qualify $contentNode $base]
            set r {}
            foreach node [getElements $contentNode] {
                set r [concat $r [parse_content $node]]
            }
            return [concat $base $r]
        }
        attribute {
            log::log debug "content attribute" 
        }
        sequence { log::log debug "content sequence" }
        choice { log::log debug "content choice" }
        element {
            set name [getElementAttribute $contentNode name]
            set type [getElementAttribute $contentNode type]
            return [list [::SOAP::WSDL::qualify $contentNode $type] $name]
        }
        default { log::log warning "unrecognised node \"$contentName\""}
    }
    return {}
}

# -------------------------------------------------------------------------

proc ::SOAP::Schema::element {element} {
    array set elt {name {} type {} maxOccurs 1 minOccurs 1 nillable 0\
                       children {}}
    set elt(name) [getElementName $elt]
    if {[string equal [parent type] all] && value != 1} {
        return -code error "invalid attribute"
    }
    set elt(maxOccurs) $maxOccurs
}

# -------------------------------------------------------------------------

package provide SOAP::Schema $::SOAP::Schema::version

# -------------------------------------------------------------------------
# Local variables:
#    indent-tabs-mode: nil
# End:
