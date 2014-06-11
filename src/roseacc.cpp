
#include "openacc_spec.hpp"

#include "MDCG/OpenACC/model.hpp"

#include "DLX/OpenACC/language.hpp"
#include "DLX/OpenACC/compiler.hpp"

#include <cassert>

int main(int argc, char ** argv) {

  std::vector<std::string> args(argv, argv + argc);
  std::string inc_opt("-I");
  args.push_back(inc_opt + LIBOPENACC_INC_PATH);
  args.push_back(inc_opt + OPENCL_INC_PATH);
  args.push_back("-DOPENACC");

  // Build ROSE project
  SgProject * project = new SgProject::SgProject(args);

  // Initialize DLX for OpenACC
  DLX::OpenACC::language_t::init(); 

  DLX::Frontend::Frontend<DLX::OpenACC::language_t> frontend;
  assert(frontend.parseDirectives(project));
  frontend.toDot(std::cout);

  std::string ocl_kernels_file("kernels.cl");
  std::string kernels_desc_file("host-data.c");
  std::string versions_db_file("versions.db");
  std::string libopenacc_inc_dir(LIBOPENACC_INC_PATH);
  std::string kernels_dir(boost::filesystem::current_path().string());
  DLX::OpenACC::compiler_modules_t compiler_modules(project, ocl_kernels_file, kernels_desc_file, versions_db_file, libopenacc_inc_dir, kernels_dir);

  DLX::Compiler::Compiler<DLX::OpenACC::language_t, DLX::OpenACC::compiler_modules_t> compiler(compiler_modules);
  assert(compiler.compile(frontend.directives, frontend.graph_entry, frontend.graph_final));

  return backend(project);
}

