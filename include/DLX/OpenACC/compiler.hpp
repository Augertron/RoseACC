
#ifndef __DLX_COMPILER_OPENACC_HPP__
#define __DLX_COMPILER_OPENACC_HPP__

#include "openacc_spec.hpp"

#include "MDCG/OpenACC/model.hpp"

namespace DLX {

namespace OpenACC {

struct compiler_modules_t {
  MFB::KLT_Driver driver;

  MDCG::ModelBuilder model_builder;
  MDCG::CodeGenerator codegen;
  KLT::Generator<Annotation, Language, Runtime, MFB::KLT_Driver> generator;

  KLT::CG_Config<Annotation, Language, Runtime> cg_config;

  unsigned libopenacc_model;
  unsigned host_data_file_id;
  std::string ocl_kernels_file;
  std::string versions_db_file;

  MDCG::Model::class_t compiler_data_class;

  MDCG::OpenACC::CompilerData::input_t comp_data;

  struct libopenacc_api_t {
    SgFunctionSymbol * push_data_environment;
    SgFunctionSymbol * pop_data_environment;

    SgFunctionSymbol * build_region;
    SgFunctionSymbol * region_execute;

    SgFunctionSymbol * copyin;
    SgFunctionSymbol * copyout;
    SgFunctionSymbol * create;
    SgFunctionSymbol * present_or_copyin;
    SgFunctionSymbol * present_or_copyout;
    SgFunctionSymbol * present_or_create;

    MDCG::Model::class_t region_class;
  } libopenacc_api;

  void loadOpenaccPrivateAPI();

  compiler_modules_t(
    SgProject * project,
    const std::string & ocl_kernels_file_,
    const std::string & kernels_desc_file_,
    const std::string & versions_db_file_,
    const std::string & libopenacc_inc_dir,
    const std::string & kernels_dir
  );
};

}

}

#endif /* __DLX_COMPILER_OPENACC_HPP__ */

