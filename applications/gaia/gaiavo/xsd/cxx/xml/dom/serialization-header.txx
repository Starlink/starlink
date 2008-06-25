// file      : xsd/cxx/xml/dom/serialization-header.txx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : Copyright (c) 2005-2008 Code Synthesis Tools CC
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

#include <vector>

#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMAttr.hpp>
#include <xercesc/dom/DOMNamedNodeMap.hpp>

#include <xercesc/util/XMLUni.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/validators/schema/SchemaSymbols.hpp>

#include <xsd/cxx/xml/string.hxx>
#include <xsd/cxx/xml/bits/literals.hxx>

namespace xsd
{
  namespace cxx
  {
    namespace xml
    {
      namespace dom
      {
        //
        //
        template <typename C>
        std::basic_string<C>
        prefix (const C* ns, const xercesc::DOMElement& e)
        {
          string xns (ns);

#if _XERCES_VERSION >= 30000
          const XMLCh* p (e.lookupPrefix (xns.c_str ()));

          if (p == 0)
          {
            // 'xml' prefix requires special handling and Xerces folks
            // refuse to handle this in DOM so I have to do it myself.
            //
            if (std::basic_string<C> (ns) == xml::bits::xml_namespace<C> ())
              return xml::bits::xml_prefix<C> ();

            throw no_prefix ();
          }
#else
          const XMLCh* p (e.lookupNamespacePrefix (xns.c_str (), false));

          if (p == 0)
          {
            if (e.isDefaultNamespace (xns.c_str ()))
            {
              return std::basic_string<C> ();
            }
            else
            {
              // 'xml' prefix requires special handling and Xerces folks
              // refuse to handle this in DOM so I have to do it myself.
              //
              if (std::basic_string<C> (ns) == xml::bits::xml_namespace<C> ())
                return xml::bits::xml_prefix<C> ();

              throw no_prefix ();
            }
          }
#endif
          return transcode<C> (p);
        }

        //
        //
        template <typename C>
        void
        clear (xercesc::DOMElement& e)
        {
          // HP aCC cannot handle using namespace xercesc;
          //
          using xercesc::DOMNode;
          using xercesc::DOMAttr;
          using xercesc::DOMNamedNodeMap;
          using xercesc::XMLString;
          using xercesc::SchemaSymbols;

          // Remove child nodes.
          //
          while (xercesc::DOMNode* n = e.getFirstChild ())
          {
            e.removeChild (n);
            n->release ();
          }

          // Remove attributes.
          //
          DOMNamedNodeMap* att_map (e.getAttributes ());
          XMLSize_t n (att_map->getLength ());

          if (n != 0)
          {
            std::vector<DOMAttr*> atts;

            // Collect all attributes to be removed while filtering
            // out special cases (xmlns & xsi).
            //
            for (XMLSize_t i (0); i != n; ++i)
            {
              DOMAttr* a (static_cast<DOMAttr*> (att_map->item (i)));
              const XMLCh* ns (a->getNamespaceURI ());

              if (ns != 0)
              {
                if (XMLString::equals (ns, xercesc::XMLUni::fgXMLNSURIName))
                  continue;

                if (XMLString::equals (ns, SchemaSymbols::fgURI_XSI))
                {
                  const XMLCh* name (a->getLocalName ());

                  if (XMLString::equals (
                        name, SchemaSymbols::fgXSI_SCHEMALOCACTION) ||
                      XMLString::equals (
                        name, SchemaSymbols::fgXSI_NONAMESPACESCHEMALOCACTION))
                    continue;
                }
              }

              atts.push_back (a);
            }

            for (std::vector<DOMAttr*>::iterator i (atts.begin ()),
                   end (atts.end ()); i != end; ++i)
            {
              e.removeAttributeNode (*i);
              (*i)->release ();
            }
          }
        }
      }
    }
  }
}

