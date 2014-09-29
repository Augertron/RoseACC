
#ifndef __KLT_RUNTIME_OPENACC_HPP__
#define __KLT_RUNTIME_OPENACC_HPP__

#include "KLT/Core/runtime.hpp"

#include "MFB/Sage/driver.hpp"

#define OPENACC_EXEC_MODE_GANG_0 0
#define OPENACC_EXEC_MODE_GANG_1 1
#define OPENACC_EXEC_MODE_GANG_2 2
#define OPENACC_EXEC_MODE_WORKER_0 3
#define OPENACC_EXEC_MODE_WORKER_1 4
#define OPENACC_EXEC_MODE_WORKER_2 5
#define OPENACC_EXEC_MODE_VECTOR 6

class SgExpression;
class SgVariableSymbol;
class SgFunctionSymbol;
class SgClassSymbol;

namespace MDCG {
namespace Model {
class model_t;
}
}

namespace KLT {

namespace Runtime {

class OpenACC {
  public:
    enum tile_kind_e {
      e_static_tile = 0,
      e_dynamic_tile = 1,
      e_gang_tile,
      e_worker_tile,
      e_vector_tile
    };

    struct tile_desc_t {
      size_t id; // ID of the tile in the kernel

      enum tile_kind_e kind;
      union {
        size_t nbr_it;
        size_t level;
      } param;

      SgVariableSymbol * iterator_sym;
    };

    struct a_loop {
      size_t id; // id of the loop in the kernel

      SgExpression * lb;
      SgExpression * ub;
      SgExpression * stride;

      std::vector<tile_desc_t> tiles;
    };

    struct exec_config_t {
      size_t num_gangs[3];
      size_t num_workers[3];
      size_t vector_length;
    };

    typedef short exec_mode_t;
    static const size_t num_exec_modes;

    static SgClassSymbol * runtime_device_context_symbol;

    struct runtime_device_function_symbols_t {
      SgFunctionSymbol * gang_iter_symbol;
      SgFunctionSymbol * worker_iter_symbol;
      SgFunctionSymbol * loop_lower_symbol;
      SgFunctionSymbol * loop_upper_symbol;
      SgFunctionSymbol * tile_length_symbol;
      SgFunctionSymbol * tile_stride_symbol;
      SgFunctionSymbol * barrier_workers_symbol;
      SgFunctionSymbol * is_master_gang_lvl_symbol;
      SgFunctionSymbol * is_master_worker_lvl_symbol;
    };
    static runtime_device_function_symbols_t runtime_device_function_symbols;

    static void loadAPI(const MDCG::Model::model_t & model);
};

}

}

#endif /* __KLT_RUNTIME_OPENACC_HPP__ */

