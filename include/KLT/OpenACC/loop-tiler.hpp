
#ifndef __OPENACC_KLT_ITMAP_HPP__
#define __OPENACC_KLT_ITMAP_HPP__

#include "openacc_spec.hpp"

#include "KLT/Core/loop-tiler.hpp"

namespace KLT {

template <>
LoopTiler<DLX::KLT_Annotation<DLX::OpenACC::language_t>, Language::OpenCL, Runtime::OpenACC>::loop_tiling_t::loop_tiling_t(LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::loop_t * loop_);

}

#endif /* __OPENACC_KLT_ITMAP_HPP__ */

