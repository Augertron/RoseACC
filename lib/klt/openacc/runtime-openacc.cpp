
#include "MDCG/model-builder.hpp"
#include "MDCG/model.hpp"
#include "MDCG/model-function.hpp"
#include "MDCG/model-class.hpp"

#include "KLT/OpenACC/runtime-openacc.hpp"
#include "MFB/Sage/api.hpp"

namespace KLT {

namespace Runtime {

OpenACC::exec_mode_e OpenACC::default_execution_mode = OpenACC::gr_ws_vs;



SgClassSymbol * OpenACC::runtime_device_context_symbol = NULL;
OpenACC::runtime_device_function_symbols_t OpenACC::runtime_device_function_symbols = { NULL, NULL };

void OpenACC::loadAPI(MFB::Driver<MFB::Sage> & mfb_driver, std::string inc_path) {
  MDCG::ModelBuilder model_builder(mfb_driver);
  unsigned model_id = model_builder.create();

  model_builder.add(model_id, "api", inc_path + "/OpenACC/device", "cl");

  const MDCG::Model::model_t & model = model_builder.get(model_id);

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

}

}

}

