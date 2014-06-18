
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
      it_annotation->clause->kind == DLX::OpenACC::language_t::e_acc_clause_vector
    ) {
      Runtime::OpenACC::tile_desc_t & tile_desc = *(tiles.insert(tiles.end(), Runtime::OpenACC::tile_desc_t()));
      switch (it_annotation->clause->kind) {
        case DLX::OpenACC::language_t::e_acc_clause_tile:
          assert(false);
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
      }
    }
  }
}

}

