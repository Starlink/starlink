// file      : xsd/cxx/parser/map.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : Copyright (c) 2005-2008 Code Synthesis Tools CC
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

#ifndef XSD_CXX_PARSER_MAP_HXX
#define XSD_CXX_PARSER_MAP_HXX

#include <map>
#include <string>

#include <xsd/cxx/ro-string.hxx>
#include <xsd/cxx/parser/elements.hxx>

namespace xsd
{
  namespace cxx
  {
    namespace parser
    {
      // Parser map. Used in the polymorphic document parsing.
      //
      template <typename C>
      struct parser_map
      {
        virtual
        ~parser_map ();

        // The type argument is the type name and namespace from the
        // xsi:type attribute or substitution group map in the form
        // "<name> <namespace>" with the space and namespace part
        // absent if the type does not have a namespace.
        //
        virtual parser_base<C>*
        find (const ro_string<C>& type) = 0;
      };


      // Parser map implementation.
      //
      template <typename C>
      struct parser_map_impl: parser_map<C>
      {
        virtual
        ~parser_map_impl ();

        parser_map_impl ()
            : map_ (0)
        {
        }

        void
        insert (const C* type, parser_base<C>&);

        virtual parser_base<C>*
        find (const ro_string<C>& type);

      private:
        parser_map_impl (const parser_map_impl&);

        parser_map_impl&
        operator= (const parser_map_impl&);

      private:
        typedef std::map<std::basic_string<C>, parser_base<C>*> map;

        map* map_;
      };
    }
  }
}

#include <xsd/cxx/parser/map.txx>

#endif  // XSD_CXX_PARSER_MAP_HXX
