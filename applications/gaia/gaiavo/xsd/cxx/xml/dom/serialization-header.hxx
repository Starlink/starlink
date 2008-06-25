// file      : xsd/cxx/xml/dom/serialization-header.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : Copyright (c) 2005-2008 Code Synthesis Tools CC
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

#ifndef XSD_CXX_XML_DOM_SERIALIZATION_HEADER_HXX
#define XSD_CXX_XML_DOM_SERIALIZATION_HEADER_HXX

#include <map>
#include <string>

#include <xercesc/dom/DOMElement.hpp>

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
        class no_prefix {};

        template <typename C>
        std::basic_string<C>
        prefix (const C* ns, const xercesc::DOMElement&);

        template <typename C>
        inline std::basic_string<C>
        prefix (const std::basic_string<C>& ns, const xercesc::DOMElement& e)
        {
          return prefix (ns.c_str (), e);
        }

        //
        //
        template <typename C>
        void
        clear (xercesc::DOMElement&);

        //
        //
        template <typename C>
        struct namespace_info
        {
          typedef std::basic_string<C> string;

          namespace_info ()
          {
          }

          namespace_info (const string& name_, const string& schema_)
              : name (name_),
                schema (schema_)
          {
          }

          std::basic_string<C> name;
          std::basic_string<C> schema;
        };


        // Map of namespace prefix to namespace_info.
        //
        template <typename C>
        struct namespace_infomap:
          public std::map<std::basic_string<C>, namespace_info<C> >
        {
        };
      }
    }
  }
}

#include <xsd/cxx/xml/dom/serialization-header.txx>

#endif  // XSD_CXX_XML_DOM_SERIALIZATION_HEADER_HXX
