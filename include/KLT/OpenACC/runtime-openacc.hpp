
#ifndef __KLT_RUNTIME_OPENACC_HPP__
#define __KLT_RUNTIME_OPENACC_HPP__

#include "MFB/Sage/driver.hpp"

class SgExpression;
class SgVariableSymbol;
class SgFunctionSymbol;
class SgClassSymbol;

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

    enum exec_mode_e {
      gr_ws_vs, /// Gang Redondant   , Worker Single      , Vector Single
      gp_ws_vs, /// Gang Partitioned , Worker Single      , Vector Single
      gr_wp_vs, /// Gang Redondant   , Worker Partitioned , Vector Single
      gp_wp_vs, /// Gang Partitioned , Worker Partitioned , Vector Single
      gr_ws_vp, /// Gang Redondant   , Worker Single      , Vector Partitioned
      gp_ws_vp, /// Gang Partitioned , Worker Single      , Vector Partitioned
      gr_wp_vp, /// Gang Redondant   , Worker Partitioned , Vector Partitioned
      gp_wp_vp  /// Gang Partitioned , Worker Partitioned , Vector Partitioned
    };

    static exec_mode_e default_execution_mode;

    static SgClassSymbol * runtime_device_context_symbol;

    struct runtime_device_function_symbols_t {
      SgFunctionSymbol * gang_iter_symbol;
      SgFunctionSymbol * worker_iter_symbol;
      SgFunctionSymbol * loop_lower_symbol;
      SgFunctionSymbol * loop_upper_symbol;
      SgFunctionSymbol * tile_length_symbol;
      SgFunctionSymbol * tile_stride_symbol;
    };
    static runtime_device_function_symbols_t runtime_device_function_symbols;

    static void loadAPI(MFB::Driver<MFB::Sage> & mfb_driver, std::string inc_path);
};

}

}

#endif /* __KLT_RUNTIME_OPENACC_HPP__ */

