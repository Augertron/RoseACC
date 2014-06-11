
#ifndef __OPENACC_MDCG_OPENACC_HPP_
#define __OPENACC_MDCG_OPENACC_HPP_

#include "openacc_spec.hpp"

#include "sqlite3.h"

namespace MDCG {

namespace OpenACC {

struct TileDesc {
  struct input_t {
    /// \todo
  };

  static SgExpression * createFieldInitializer(
    const MDCG::CodeGenerator & codegen,
    MDCG::Model::field_t element,
    unsigned field_id,
    const input_t & input,
    unsigned file_id
  );

  static void storeToDB(sqlite3 * db_file, const input_t & input);
};

struct LoopDesc {
  typedef Runtime::a_loop input_t;

  static SgExpression * createFieldInitializer(
    const MDCG::CodeGenerator & codegen,
    MDCG::Model::field_t element,
    unsigned field_id,
    const input_t & input,
    unsigned file_id
  );

  static void storeToDB(sqlite3 * db_file, unsigned region_id, unsigned kernel_id, unsigned version_id, unsigned loop_id, const input_t & input);
};

struct KernelVersion {
  typedef Kernel::a_kernel * input_t;

  static SgExpression * createFieldInitializer(
    const MDCG::CodeGenerator & codegen,
    MDCG::Model::field_t element,
    unsigned field_id,
    const input_t & input,
    unsigned file_id
  );

  static void storeToDB(sqlite3 * db_file, unsigned region_id, unsigned kernel_id, unsigned version_id, const input_t & input);
};

SgExpression * createArrayOfTypeSize(
  const MDCG::CodeGenerator & codegen,
  const std::list<SgVariableSymbol *> & input,
  std::string array_name,
  unsigned file_id
);

struct KernelDesc {
  typedef Kernel * input_t;

  static SgExpression * createFieldInitializer(
    const MDCG::CodeGenerator & codegen,
    MDCG::Model::field_t element,
    unsigned field_id,
    const input_t & input,
    unsigned file_id
  );

  static void storeToDB(sqlite3 * db_file, unsigned region_id, unsigned kernel_id, const input_t & input);
};

struct KernelWithDepsDesc {
  typedef Kernel * input_t;

  static SgExpression * createFieldInitializer(
    const MDCG::CodeGenerator & codegen,
    MDCG::Model::field_t element,
    unsigned field_id,
    const input_t & input,
    unsigned file_id
  );

  static void storeToDB(sqlite3 * db_file, const input_t & input);
};

struct KernelGroupDesc {
  typedef std::list<Kernel *> input_t;

  static SgExpression * createFieldInitializer(
    const MDCG::CodeGenerator & codegen,
    MDCG::Model::field_t element,
    unsigned field_id,
    const input_t & input,
    unsigned file_id
  );

  static void storeToDB(sqlite3 * db_file, const input_t & input);
};

struct RegionDesc {
  struct input_t {
    unsigned id;
    std::string file;
    LoopTrees * loop_tree;
    std::set<std::list<Kernel *> > kernel_lists;
  };

  static SgExpression * createFieldInitializer(
    const MDCG::CodeGenerator & codegen,
    MDCG::Model::field_t element,
    unsigned field_id,
    const input_t & input,
    unsigned file_id
  );

  static void storeToDB(sqlite3 * db_file, const input_t & input);
};

struct CompilerData {
  struct input_t {
    SgExpression * runtime_dir;
    SgExpression * ocl_runtime;
    SgExpression * kernels_dir;
    std::vector<RegionDesc::input_t> regions;
  };

  static SgExpression * createFieldInitializer(
    const MDCG::CodeGenerator & codegen,
    MDCG::Model::field_t element,
    unsigned field_id,
    const input_t & input,
    unsigned file_id
  );

  static void storeToDB(const std::string & db_file, const input_t & input);
};

unsigned readOpenaccModel(MDCG::ModelBuilder & model_builder, const std::string & libopenacc_inc_dir);

}

}

#endif /* __OPENACC_MDCG_OPENACC_HPP_ */

