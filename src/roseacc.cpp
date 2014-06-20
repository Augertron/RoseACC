
#include "openacc_spec.hpp"

#include "MDCG/OpenACC/model.hpp"

#include "DLX/OpenACC/language.hpp"
#include "DLX/OpenACC/compiler.hpp"

#include <cassert>

int main(int argc, char ** argv) {

  std::vector<std::string> args(argv, argv + argc);

  std::vector<std::string> roseacc_args;
  std::vector<std::string>::iterator it_str = args.begin();
  while (it_str != args.end()) {
    if (it_str->find("--roseacc:") == 0) {
      roseacc_args.push_back(it_str->substr(10));
      it_str = args.erase(it_str);
    }
    else it_str++;
  }

  std::string inc_opt("-I");
  args.push_back(inc_opt + LIBOPENACC_PATH + "/include");
  std::string opencl_inc_path(OPENCL_INC_PATH);
  if (opencl_inc_path != "/usr/include")
    args.push_back(inc_opt + opencl_inc_path);
  args.push_back("-DOPENACC");

  // Build ROSE project
  SgProject * project = new SgProject::SgProject(args);

  std::string ocl_kernels_file(".cl");
  std::string kernels_desc_file("-data.c");
  std::string versions_db_file(".db");

  std::string cg_prefix;
  enum descriptor_format_e {
    e_unknown,
    e_static_data,
    e_database
  } descriptor_format = e_unknown;
  for (it_str = roseacc_args.begin(); it_str != roseacc_args.end(); it_str++) {
    if (it_str->find("desc_format") == 0) {
      assert(descriptor_format == e_unknown);
      std::string format = it_str->substr(12);
      if (format.compare("static_data"))
        descriptor_format = e_static_data;
      else if (format.compare("database"))
        descriptor_format = e_database;
      else assert(false);
    }
    else if (it_str->find("cg_prefix") == 0) {
      assert(cg_prefix.empty());
      cg_prefix = it_str->substr(10);
    }
    else assert(false);
  }
  assert(descriptor_format != e_unknown);

  if (cg_prefix.empty()) {
    assert(project->get_fileList().size() == 1);
    SgSourceFile * target_file = isSgSourceFile(project->get_fileList()[0]);
    assert(target_file != NULL);
    cg_prefix = target_file->get_sourceFileNameWithoutPath();
    size_t dot_pos = cg_prefix.find(".");
    assert(dot_pos > 0 && dot_pos != std::string::npos);
    cg_prefix = cg_prefix.substr(0, dot_pos);
  }

  assert(!cg_prefix.empty());

  ocl_kernels_file = cg_prefix + ocl_kernels_file;
  kernels_desc_file = cg_prefix + kernels_desc_file;
  versions_db_file = cg_prefix + versions_db_file;

  // Initialize DLX for OpenACC
  DLX::OpenACC::language_t::init(); 

  DLX::Frontend::Frontend<DLX::OpenACC::language_t> frontend;
  assert(frontend.parseDirectives(project));
//frontend.toDot(std::cout);

  std::string libopenacc_dir(LIBOPENACC_PATH);
  std::string kernels_dir(boost::filesystem::current_path().string());
  DLX::OpenACC::compiler_modules_t compiler_modules(project, ocl_kernels_file, kernels_desc_file, versions_db_file, libopenacc_dir, kernels_dir);

  DLX::Compiler::Compiler<DLX::OpenACC::language_t, DLX::OpenACC::compiler_modules_t> compiler(compiler_modules);
  assert(compiler.compile(frontend.directives, frontend.graph_entry, frontend.graph_final));

  return backend(project);
}

