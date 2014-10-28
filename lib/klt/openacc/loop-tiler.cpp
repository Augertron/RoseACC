
#include "KLT/OpenACC/loop-tiler.hpp"

#include <vector>

namespace KLT {

template <>
LoopTiler<DLX::KLT_Annotation<DLX::OpenACC::language_t>, Language::OpenCL, Runtime::OpenACC>::loop_tiling_t::loop_tiling_t(LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::loop_t * loop_) :
  loop(loop_),
  tiles()
{
  std::vector<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::const_iterator it_annotation;
  for (it_annotation = loop->annotations.begin(); it_annotation != loop->annotations.end(); it_annotation++) {
    if (
      it_annotation->clause->kind == DLX::OpenACC::language_t::e_acc_clause_tile   ||
      it_annotation->clause->kind == DLX::OpenACC::language_t::e_acc_clause_gang   ||
      it_annotation->clause->kind == DLX::OpenACC::language_t::e_acc_clause_worker ||
      it_annotation->clause->kind == DLX::OpenACC::language_t::e_acc_clause_vector ||
      it_annotation->clause->kind == DLX::OpenACC::language_t::e_acc_clause_seq    ||
      (it_annotation->clause->kind == DLX::OpenACC::language_t::e_acc_clause_split && loop->annotations.size() == 1)
    ) {
      Runtime::OpenACC::tile_desc_t & tile_desc = *(tiles.insert(tiles.end(), Runtime::OpenACC::tile_desc_t()));
      switch (it_annotation->clause->kind) {
        case DLX::OpenACC::language_t::e_acc_clause_tile:
        {
          switch (((DLX::Directives::clause_t<DLX::OpenACC::language_t, DLX::OpenACC::language_t::e_acc_clause_tile> *)(it_annotation->clause))->parameters.kind) {
            case DLX::Directives::generic_clause_t<DLX::OpenACC::language_t>::parameters_t<DLX::OpenACC::language_t::e_acc_clause_tile>::e_static_tile:
              tile_desc.kind = Runtime::OpenACC::e_static_tile;
              tile_desc.param.nbr_it = ((DLX::Directives::clause_t<DLX::OpenACC::language_t, DLX::OpenACC::language_t::e_acc_clause_tile> *)(it_annotation->clause))->parameters.nbr_it;
              break;
            case DLX::Directives::generic_clause_t<DLX::OpenACC::language_t>::parameters_t<DLX::OpenACC::language_t::e_acc_clause_tile>::e_dynamic_tile:
              tile_desc.kind = Runtime::OpenACC::e_dynamic_tile;
              tile_desc.param.nbr_it = 0;
              break;
            default:
              assert(false);
          }
          break;
        }
        case DLX::OpenACC::language_t::e_acc_clause_gang:
        {
           tile_desc.kind = Runtime::OpenACC::e_gang_tile;
           tile_desc.param.level = ((DLX::Directives::clause_t<DLX::OpenACC::language_t, DLX::OpenACC::language_t::e_acc_clause_gang> *)(it_annotation->clause))->parameters.lvl;
           break;
        }
        case DLX::OpenACC::language_t::e_acc_clause_worker:
        {
           tile_desc.kind = Runtime::OpenACC::e_worker_tile;
           tile_desc.param.level = ((DLX::Directives::clause_t<DLX::OpenACC::language_t, DLX::OpenACC::language_t::e_acc_clause_worker> *)(it_annotation->clause))->parameters.lvl;
           break;
        }
        case DLX::OpenACC::language_t::e_acc_clause_vector:
        {
           tile_desc.kind = Runtime::OpenACC::e_vector_tile;
           tile_desc.param.level = 0;
           break;
        }
        case DLX::OpenACC::language_t::e_acc_clause_seq:
        {
          // Insert a dynamic tile for loop marked as sequential (more for homogeneity than anything else)
          tile_desc.kind = Runtime::OpenACC::e_dynamic_tile;
          tile_desc.param.nbr_it = 0;
          break;
        }
        case DLX::OpenACC::language_t::e_acc_clause_split:
        {
          // If a split clause is present and no gang nor worker (test for that has to be improved) then we need the loop to appear in the context so we create a dynamic tile
          tile_desc.kind = Runtime::OpenACC::e_dynamic_tile;
          tile_desc.param.nbr_it = 0;
          break;
        }
      }
    }
  }
}

}

