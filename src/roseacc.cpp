
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
    else if (it_str->find("-roseacc:") == 0) {
      roseacc_args.push_back(it_str->substr(9));
      it_str = args.erase(it_str);
    }
    else it_str++;
  }

  std::string inc_opt("-I");
  args.push_back(inc_opt + OPENACC_INC_PATH);
  std::string opencl_inc_path(OPENCL_INC_PATH);
  if (opencl_inc_path != "/usr/include")
    args.push_back(inc_opt + opencl_inc_path);
  args.push_back("-DOPENACC");
//args.push_back("-rose:wave");

  // Build ROSE project
  SgProject * project = new SgProject::SgProject(args);

  std::string ocl_kernels_file(".cl");
  std::string kernels_desc_file("-data.c");
  std::string versions_db_file(".db");

  std::string cg_prefix;
  enum descriptor_format_e {
    e_static_data,
    e_database
  } descriptor_format = e_static_data;
  bool compile_switch;
  for (it_str = roseacc_args.begin(); it_str != roseacc_args.end(); it_str++) {
    if (it_str->find("desc_format") == 0) {
      std::string format = it_str->substr(12);
      if (format.compare("static_data") == 0)
        descriptor_format = e_static_data;
      else if (format.compare("database") == 0)
        descriptor_format = e_database;
      else assert(false);
    }
    else if (it_str->find("cg_prefix") == 0) {
      assert(cg_prefix.empty());
      cg_prefix = it_str->substr(10);
    }
    else if (it_str->find("compile") == 0) {
      std::string compile_switch_str = it_str->substr(8);
      if (compile_switch_str.compare("true") == 0)
        compile_switch = true;
      else if (compile_switch_str.compare("false") == 0)
        compile_switch = false;
      else assert(false);
    }
    else {
      std::cout << "RoseACC option : \"" << *it_str << "\" unrecognized." << std::endl;
      assert(false);
    }
  }
  assert(descriptor_format == e_static_data); // DB storage broken...

  if (cg_prefix.empty()) {
//    assert(project->get_fileList().size() == 1);
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
  std::string openacc_inc_path(OPENACC_INC_PATH);
  std::string openacc_lib_path(OPENACC_LIB_PATH);
  std::string kernels_dir(boost::filesystem::current_path().string());

  // Initialize OpenACC language and build compiler module (loads OpenACC API)
  DLX::OpenACC::language_t::init();
  DLX::OpenACC::compiler_modules_t compiler_modules(project, ocl_kernels_file, kernels_desc_file, versions_db_file, openacc_inc_path, openacc_lib_path, kernels_dir);

  DLX::Frontend::Frontend<DLX::OpenACC::language_t> frontend;
  bool res = frontend.parseDirectives(project);
  assert(res);

//frontend.toDot(std::cout);

  DLX::Compiler::Compiler<DLX::OpenACC::language_t, DLX::OpenACC::compiler_modules_t> compiler(compiler_modules);
  res = compiler.compile(frontend.directives, frontend.graph_entry, frontend.graph_final);
  assert(res);

  switch (descriptor_format) {
    case e_static_data:
      compiler_modules.static_initializer.addDeclaration<MDCG::OpenACC::CompilerData>(
                                   compiler_modules.compiler_data_class,
                                   compiler_modules.comp_data,
                                   compiler_modules.host_data_file_id,
                                   "compiler_data"
                                 );
      break;
    case e_database:
      assert(false);
  /// \todo empty 'compiler_data'
  /// \todo MDCG::OpenACC::CompilerData::storeToDB(compiler_modules.versions_db_file, compiler_modules.comp_data);
      break;
    default:
      res = false;
      assert(false);
  }
  if (res && compile_switch)
    return backend(project);
  else if (res && !compile_switch) {
    project->unparse();
    return 0;
  }
  else return 1;
}

