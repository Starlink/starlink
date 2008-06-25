// file      : xsd/cxx/parser/map.txx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : Copyright (c) 2005-2008 Code Synthesis Tools CC
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

namespace xsd
{
  namespace cxx
  {
    namespace parser
    {
      // parser_map
      //
      template <typename C>
      parser_map<C>::
      ~parser_map ()
      {
      }

      // parser_map_impl
      //
      template <typename C>
      parser_map_impl<C>::
      ~parser_map_impl ()
      {
        delete map_;
      }

      template <typename C>
      void parser_map_impl<C>::
      insert (const C* type, parser_base<C>& parser)
      {
        if (map_ == 0)
          map_ = new map;

        (*map_)[type] = &parser;
      }

      template <typename C>
      parser_base<C>* parser_map_impl<C>::
      find (const ro_string<C>& type)
      {
        if (map_ == 0)
          return 0;

        typename map::const_iterator i (map_->find (type));

        return i != map_->end () ? i->second : 0;
      }
    }
  }
}
