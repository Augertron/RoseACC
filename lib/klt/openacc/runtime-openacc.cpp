
#include "MDCG/model-builder.hpp"
#include "MDCG/model.hpp"
#include "MDCG/model-function.hpp"
#include "MDCG/model-class.hpp"

#include "KLT/Core/kernel.hpp"
#include "KLT/OpenACC/language-opencl.hpp"
#include "KLT/OpenACC/runtime-openacc.hpp"
#include "KLT/OpenACC/dlx-openacc.hpp"
#include "MFB/Sage/api.hpp"

#include "DLX/Core/directives.hpp"
#include "DLX/Core/clauses.hpp"
#include "DLX/OpenACC/language.hpp"

namespace KLT {

namespace Runtime {

template <>
void get_exec_config<
  DLX::KLT_Annotation<DLX::OpenACC::language_t>,
  Language::OpenCL,
  Runtime::OpenACC
> (
  OpenACC::exec_config_t & exec_config,
  const Kernel<DLX::KLT_Annotation<DLX::OpenACC::language_t>, Language::OpenCL, Runtime::OpenACC> * kernel
) {
  exec_config.num_gangs[0] = 1;
  exec_config.num_gangs[1] = 1;
  exec_config.num_gangs[2] = 1;
  exec_config.num_workers[0] = 1;
  exec_config.num_workers[1] = 1;
  exec_config.num_workers[2] = 1;
  exec_config.vector_length = 1;

  const std::vector<LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::loop_t *> & loops = kernel->getLoops();
  std::vector<LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::loop_t *>::const_iterator it_loop;
  std::vector<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::const_iterator it_annotation;
  for (it_loop = loops.begin(); it_loop != loops.end(); it_loop++) {
    for (it_annotation = (*it_loop)->annotations.begin(); it_annotation != (*it_loop)->annotations.end(); it_annotation++) {
      DLX::Directives::generic_clause_t<DLX::OpenACC::language_t> * clause = it_annotation->clause;
      switch (clause->kind) {
        case DLX::OpenACC::language_t::e_acc_clause_gang:
          exec_config.num_gangs[((DLX::Directives::clause_t<DLX::OpenACC::language_t, DLX::OpenACC::language_t::e_acc_clause_gang> *)clause)->parameters.lvl] = 0;
          break;
        case DLX::OpenACC::language_t::e_acc_clause_worker:
          exec_config.num_workers[((DLX::Directives::clause_t<DLX::OpenACC::language_t, DLX::OpenACC::language_t::e_acc_clause_worker> *)clause)->parameters.lvl] = 0;
          break;
        case DLX::OpenACC::language_t::e_acc_clause_vector:
          exec_config.vector_length = 0;
          break;
      }
    }
  }

  for (it_annotation = kernel->getLoopTree().annotations.begin(); it_annotation != kernel->getLoopTree().annotations.end(); it_annotation++) {
    DLX::Directives::generic_clause_t<DLX::OpenACC::language_t> * clause = it_annotation->clause;
    switch (clause->kind) {
      case DLX::OpenACC::language_t::e_acc_clause_num_gangs:
        /// \todo look for parameter known at compile time
        break;
      case DLX::OpenACC::language_t::e_acc_clause_num_workers:
        /// \todo look for parameter known at compile time
        break;
      case DLX::OpenACC::language_t::e_acc_clause_vector_length:
        /// \todo look for parameter known at compile time
        break;
    }
  }
}
	
SgClassSymbol * OpenACC::runtime_device_context_symbol = NULL;
OpenACC::runtime_device_function_symbols_t OpenACC::runtime_device_function_symbols = { NULL, NULL };

const size_t OpenACC::num_exec_modes = 7;

void OpenACC::loadAPI(const MDCG::Model::model_t & model) {
  MDCG::Model::class_t context_class = model.lookup<MDCG::Model::class_t>("acc_context_t_");
  runtime_device_context_symbol = context_class->node->symbol;
  assert(runtime_device_context_symbol != NULL);

  MDCG::Model::function_t gang_iter_func = model.lookup<MDCG::Model::function_t>("acc_gang_iteration");
  runtime_device_function_symbols.gang_iter_symbol = gang_iter_func->node->symbol;
  assert(runtime_device_function_symbols.gang_iter_symbol != NULL);

  MDCG::Model::function_t worker_iter_func = model.lookup<MDCG::Model::function_t>("acc_worker_iteration");
  runtime_device_function_symbols.worker_iter_symbol = worker_iter_func->node->symbol;
  assert(runtime_device_function_symbols.worker_iter_symbol != NULL);

  MDCG::Model::function_t loop_lower_func = model.lookup<MDCG::Model::function_t>("acc_get_loop_lower");
  runtime_device_function_symbols.loop_lower_symbol = loop_lower_func->node->symbol;
  assert(runtime_device_function_symbols.loop_lower_symbol != NULL);

  MDCG::Model::function_t loop_upper_func = model.lookup<MDCG::Model::function_t>("acc_get_loop_upper");
  runtime_device_function_symbols.loop_upper_symbol = loop_upper_func->node->symbol;
  assert(runtime_device_function_symbols.loop_upper_symbol != NULL);

  MDCG::Model::function_t tile_length_func = model.lookup<MDCG::Model::function_t>("acc_get_tile_length");
  runtime_device_function_symbols.tile_length_symbol = tile_length_func->node->symbol;
  assert(runtime_device_function_symbols.tile_length_symbol != NULL);

  MDCG::Model::function_t tile_stride_func = model.lookup<MDCG::Model::function_t>("acc_get_tile_stride");
  runtime_device_function_symbols.tile_stride_symbol = tile_stride_func->node->symbol;
  assert(runtime_device_function_symbols.tile_stride_symbol != NULL);

  MDCG::Model::function_t barrier_workers_func = model.lookup<MDCG::Model::function_t>("acc_barrier_workers");
  runtime_device_function_symbols.barrier_workers_symbol = barrier_workers_func->node->symbol;
  assert(runtime_device_function_symbols.barrier_workers_symbol != NULL);

  MDCG::Model::function_t is_master_gang_lvl_func = model.lookup<MDCG::Model::function_t>("acc_is_master_gang_lvl");
  runtime_device_function_symbols.is_master_gang_lvl_symbol = is_master_gang_lvl_func->node->symbol;
  assert(runtime_device_function_symbols.is_master_gang_lvl_symbol != NULL);

  MDCG::Model::function_t is_master_worker_lvl_func = model.lookup<MDCG::Model::function_t>("acc_is_master_worker_lvl");
  runtime_device_function_symbols.is_master_worker_lvl_symbol = is_master_worker_lvl_func->node->symbol;
  assert(runtime_device_function_symbols.is_master_worker_lvl_symbol != NULL);

}

}

}

