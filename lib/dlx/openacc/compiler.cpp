/*!
 * 
 * \file lib/openacc/compiler.cpp
 *
 * \author Tristan Vanderbruggen
 *
 */

#include "DLX/Core/compiler.hpp"
#include "DLX/OpenACC/language.hpp"
#include "DLX/OpenACC/compiler.hpp"

#include "KLT/Core/loop-trees.hpp"
#include "KLT/Core/loop-tiler.hpp"

#include "DLX/Core/parser.hpp"

namespace DLX {

namespace OpenACC {

/*!
 * \addtogroup grp_dlx_openacc_compiler
 * @{
 */

compiler_modules_t::compiler_modules_t(
  SgProject * project,
  const std::string & ocl_kernels_file_,
  const std::string & kernels_desc_file_,
  const std::string & versions_db_file_,
  const std::string & libopenacc_inc_dir,
  const std::string & kernels_dir
) :
  driver(project),
  model_builder(driver),
  codegen(driver),
  generator(driver, ocl_kernels_file_),
  cg_config(
    new KLT::LoopMapper<Annotation, Language, Runtime>(),
    new KLT::LoopTiler<Annotation, Language, Runtime>(),
    new KLT::DataFlow<Annotation, Language, Runtime>()
  ),
  libopenacc_model(0),
  host_data_file_id(0),
  ocl_kernels_file(ocl_kernels_file_),
  versions_db_file(versions_db_file_),
  compiler_data_class(NULL),
  comp_data(),
  libopenacc_api()
{
  host_data_file_id = driver.add(boost::filesystem::path(kernels_desc_file_));
    driver.setUnparsedFile(host_data_file_id);

  libopenacc_model = MDCG::OpenACC::readOpenaccModel(model_builder, libopenacc_inc_dir);

  // Get base class for host data generation
  std::set<MDCG::Model::class_t> classes;
  model_builder.get(libopenacc_model).lookup<MDCG::Model::class_t>("acc_compiler_data_t_", classes);
  assert(classes.size() == 1);
  compiler_data_class = *(classes.begin());

  comp_data.runtime_dir = SageBuilder::buildStringVal(libopenacc_inc_dir);
  comp_data.ocl_runtime = SageBuilder::buildStringVal("lib/opencl/libopenacc.cl");
  comp_data.kernels_dir = SageBuilder::buildStringVal(kernels_dir);

  // Load libOpenACC API for KLT
  KLT::Runtime::OpenACC::loadAPI(model_builder.get(libopenacc_model));

  // Load libOpenACC API for transformation
  loadOpenaccPrivateAPI();
}

void compiler_modules_t::loadOpenaccPrivateAPI() {
  const MDCG::Model::model_t & model = model_builder.get(libopenacc_model);
  MDCG::Model::function_t func;

  func = model.lookup<MDCG::Model::function_t>("acc_push_data_environment");
  libopenacc_api.push_data_environment = func->node->symbol;
  assert(libopenacc_api.push_data_environment != NULL);

  func = model.lookup<MDCG::Model::function_t>("acc_pop_data_environment");
  libopenacc_api.pop_data_environment = func->node->symbol;
  assert(libopenacc_api.pop_data_environment != NULL);

  func = model.lookup<MDCG::Model::function_t>("acc_build_region");
  libopenacc_api.build_region = func->node->symbol;
  assert(libopenacc_api.build_region != NULL);

  func = model.lookup<MDCG::Model::function_t>("acc_region_execute");
  libopenacc_api.region_execute = func->node->symbol;
  assert(libopenacc_api.region_execute != NULL);

  func = model.lookup<MDCG::Model::function_t>("acc_copyin_regions_");
  libopenacc_api.copyin= func->node->symbol;
  assert(libopenacc_api.copyin!= NULL);

  func = model.lookup<MDCG::Model::function_t>("acc_copyout_regions_");
  libopenacc_api.copyout= func->node->symbol;
  assert(libopenacc_api.copyout!= NULL);

  func = model.lookup<MDCG::Model::function_t>("acc_create_regions_");
  libopenacc_api.create= func->node->symbol;
  assert(libopenacc_api.create!= NULL);

  func = model.lookup<MDCG::Model::function_t>("acc_present_or_copyin_regions_");
  libopenacc_api.present_or_copyin= func->node->symbol;
  assert(libopenacc_api.present_or_copyin!= NULL);

  func = model.lookup<MDCG::Model::function_t>("acc_present_or_copyout_regions_");
  libopenacc_api.present_or_copyout= func->node->symbol;
  assert(libopenacc_api.present_or_copyout!= NULL);

  func = model.lookup<MDCG::Model::function_t>("acc_present_or_create_regions_");
  libopenacc_api.present_or_create= func->node->symbol;
  assert(libopenacc_api.present_or_create!= NULL);

  std::set<MDCG::Model::class_t> classes;
  model_builder.get(libopenacc_model).lookup<MDCG::Model::class_t>("acc_region_t_", classes);
  assert(classes.size() == 1);
  libopenacc_api.region_class = *(classes.begin());
}

}

namespace Compiler {

void translateDataSections(
  const std::vector<Frontend::data_sections_t> & data_sections,
  Directives::generic_clause_t<OpenACC::language_t> * clause,
  LoopTrees * loop_tree
) {
  std::vector<Frontend::data_sections_t>::const_iterator it_data;
  for (it_data = data_sections.begin(); it_data != data_sections.end(); it_data++) {
    KLT::Data<KLT_Annotation<OpenACC::language_t> > * data = new KLT::Data<KLT_Annotation<OpenACC::language_t> >(it_data->first, it_data->second.size());
    assert(data != NULL);
    loop_tree->addData(data);

    data->annotations.push_back(KLT_Annotation<OpenACC::language_t>(clause));

    std::vector<Frontend::section_t>::const_iterator it_section;
    for (it_section = it_data->second.begin(); it_section != it_data->second.end(); it_section++) {
      KLT::Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::section_t section;
      section.lower_bound = it_section->lower_bound;
      section.size = it_section->size;
      section.stride = it_section->stride;
      data->addSection(section);

      std::vector<SgVarRefExp *>::const_iterator it_var_ref;
      std::vector<SgVarRefExp *> var_refs;

      if (section.lower_bound != NULL) {
        var_refs = SageInterface::querySubTree<SgVarRefExp>(section.lower_bound);
        for (it_var_ref = var_refs.begin(); it_var_ref != var_refs.end(); it_var_ref++)
          loop_tree->addParameter((*it_var_ref)->get_symbol());
      }
      if (section.size != NULL) {
        var_refs = SageInterface::querySubTree<SgVarRefExp>(section.size);
        for (it_var_ref = var_refs.begin(); it_var_ref != var_refs.end(); it_var_ref++)
          loop_tree->addParameter((*it_var_ref)->get_symbol());
      }
      if (section.stride != NULL) {
        var_refs = SageInterface::querySubTree<SgVarRefExp>(section.stride);
        for (it_var_ref = var_refs.begin(); it_var_ref != var_refs.end(); it_var_ref++)
          loop_tree->addParameter((*it_var_ref)->get_symbol());
      }
    }
  }
}

void interpretClauses(
  const std::vector<Directives::generic_clause_t<OpenACC::language_t> *> & clauses,
  LoopTrees * loop_tree
) {
  std::vector<Directives::generic_clause_t<OpenACC::language_t> *>::const_iterator it_clause;
  for (it_clause = clauses.begin(); it_clause != clauses.end(); it_clause++) {
    switch ((*it_clause)->kind) {
      case OpenACC::language_t::e_acc_clause_if:
      {
        assert(false); /// \todo
        break;
      }
      case OpenACC::language_t::e_acc_clause_async:
      {
        assert(false); /// \todo
        break;
      }
      case OpenACC::language_t::e_acc_clause_num_gangs:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_num_gangs> * clause =
                 (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_num_gangs> *)(*it_clause);
        loop_tree->setNumGangs(clause->parameters.lvl, clause->parameters.exp);
        break;
      }
      case OpenACC::language_t::e_acc_clause_num_workers:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_num_workers> * clause =
                 (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_num_workers> *)(*it_clause);
        loop_tree->setNumWorkers(clause->parameters.lvl, clause->parameters.exp);
        break;
      }
      case OpenACC::language_t::e_acc_clause_vector_length:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_vector_length> * clause =
                 (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_vector_length> *)(*it_clause);
        loop_tree->setVectorLength(clause->parameters.exp);
        break;
      }
      case OpenACC::language_t::e_acc_clause_reduction:
      {
        assert(false); /// \todo
        break;
      }
      case OpenACC::language_t::e_acc_clause_copy:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_copy> * clause =
                 (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_copy> *)(*it_clause);
        translateDataSections(clause->parameters.data_sections, clause, loop_tree);
        break;
      }
      case OpenACC::language_t::e_acc_clause_copyin:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_copyin> * clause =
                 (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_copyin> *)(*it_clause);
        translateDataSections(clause->parameters.data_sections, clause, loop_tree);
        break;
      }
      case OpenACC::language_t::e_acc_clause_copyout:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_copyout> * clause =
                 (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_copyout> *)(*it_clause);
        translateDataSections(clause->parameters.data_sections, clause, loop_tree);
        break;
      }
      case OpenACC::language_t::e_acc_clause_create:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_create> * clause =
                 (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_create> *)(*it_clause);
        translateDataSections(clause->parameters.data_sections, clause, loop_tree);
        break;
      }
      case OpenACC::language_t::e_acc_clause_present:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present> * clause =
                 (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present> *)(*it_clause);
        translateDataSections(clause->parameters.data_sections, clause, loop_tree);
        break;
      }
      case OpenACC::language_t::e_acc_clause_present_or_copy:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present_or_copy> * clause =
                 (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present_or_copy> *)(*it_clause);
        translateDataSections(clause->parameters.data_sections, clause, loop_tree);
        break;
      }
      case OpenACC::language_t::e_acc_clause_present_or_copyin:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present_or_copyin> * clause =
                 (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present_or_copyin> *)(*it_clause);
        translateDataSections(clause->parameters.data_sections, clause, loop_tree);
        break;
      }
      case OpenACC::language_t::e_acc_clause_present_or_copyout:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present_or_copyout> * clause =
                 (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present_or_copyout> *)(*it_clause);
        translateDataSections(clause->parameters.data_sections, clause, loop_tree);
        break;
      }
      case OpenACC::language_t::e_acc_clause_present_or_create:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present_or_create> * clause =
                 (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present_or_create> *)(*it_clause);
        translateDataSections(clause->parameters.data_sections, clause, loop_tree);
        break;
      }
      case OpenACC::language_t::e_acc_clause_deviceptr:
      {
        assert(false); /// \todo
        break;
      }
      case OpenACC::language_t::e_acc_clause_private:
      {
        assert(false); /// \todo
        break;
      }
      case OpenACC::language_t::e_acc_clause_firstprivate:
      {
        assert(false); /// \todo
        break;
      }
      default:
        assert(false);
    }
  }
}

LoopTrees::node_t * buildLoopTree(SgStatement * stmt, LoopTrees * loop_tree, std::vector<SgVariableSymbol *> & iterators, std::vector<SgVariableSymbol *> & others, std::map<SgForStatement *, LoopTrees::loop_t *> & loop_map);

LoopTrees::block_t * buildLoopTreeBlock(SgStatement * stmt, LoopTrees * loop_tree, std::vector<SgVariableSymbol *> & iterators, std::vector<SgVariableSymbol *> & others, std::map<SgForStatement *, LoopTrees::loop_t *> & loop_map) {
  LoopTrees::node_t * child = buildLoopTree(stmt, loop_tree, iterators, others, loop_map);
  assert(child != NULL);
  LoopTrees::block_t * block = dynamic_cast<LoopTrees::block_t *>(child);
  if (block == NULL) {
    block = new LoopTrees::block_t();
    block->children.push_back(child);
  }
  return block;
}

LoopTrees::node_t * buildLoopTree(SgStatement * stmt, LoopTrees * loop_tree, std::vector<SgVariableSymbol *> & iterators, std::vector<SgVariableSymbol *> & others, std::map<SgForStatement *, LoopTrees::loop_t *> & loop_map) {
  switch (stmt->variantT()) {
    case V_SgBasicBlock:
    {
      SgBasicBlock * bb = (SgBasicBlock *)stmt;

      LoopTrees::block_t * block = new LoopTrees::block_t();

      std::vector<SgStatement *>::const_iterator it_stmt;
      for (it_stmt = bb->get_statements().begin(); it_stmt != bb->get_statements().end(); it_stmt++)
        if (!isSgPragmaDeclaration(*it_stmt))
          block->children.push_back(buildLoopTree(*it_stmt, loop_tree, iterators, others, loop_map));

      return block;
    }
    case V_SgForStatement:
    {
      SgForStatement * for_stmt = (SgForStatement *)stmt;

      SgVariableSymbol * iterator = NULL;
      SgExpression * lower_bound = NULL;
      SgExpression * upper_bound = NULL;
      SgExpression * stride = NULL;
      assert(SageInterface::getForLoopInformations(for_stmt, iterator, lower_bound, upper_bound, stride));

      iterators.push_back(iterator);
      std::vector<SgVarRefExp *>::const_iterator it_var_ref;
      std::vector<SgVarRefExp *> var_refs;

      var_refs = SageInterface::querySubTree<SgVarRefExp>(for_stmt->get_for_init_stmt());
      for (it_var_ref = var_refs.begin(); it_var_ref != var_refs.end(); it_var_ref++) {
        SgVariableSymbol * sym = (*it_var_ref)->get_symbol();
        if (std::find(iterators.begin(), iterators.end(), sym) == iterators.end())
          loop_tree->addParameter(sym); // in a loop : !iterator => parameter
      }

      var_refs = SageInterface::querySubTree<SgVarRefExp>(for_stmt->get_test());
      for (it_var_ref = var_refs.begin(); it_var_ref != var_refs.end(); it_var_ref++) {
        SgVariableSymbol * sym = (*it_var_ref)->get_symbol();
        if (std::find(iterators.begin(), iterators.end(), sym) == iterators.end())
          loop_tree->addParameter(sym); // in a loop : !iterator => parameter
      }

      var_refs = SageInterface::querySubTree<SgVarRefExp>(for_stmt->get_increment());
      for (it_var_ref = var_refs.begin(); it_var_ref != var_refs.end(); it_var_ref++) {
        SgVariableSymbol * sym = (*it_var_ref)->get_symbol();
        if (std::find(iterators.begin(), iterators.end(), sym) == iterators.end())
          loop_tree->addParameter(sym); // in a loop : !iterator => parameter
      }

      LoopTrees::loop_t * loop = new LoopTrees::loop_t(iterator, lower_bound, upper_bound, stride);

      loop_map.insert(std::pair<SgForStatement *, LoopTrees::loop_t *>(for_stmt, loop));

      loop->block = buildLoopTreeBlock(for_stmt->get_loop_body(), loop_tree, iterators, others, loop_map);

      return loop;
    }
    case V_SgIfStmt:
    {
      SgIfStmt * if_stmt = (SgIfStmt *)stmt;

      SgExprStatement * cond_stmt = isSgExprStatement(if_stmt->get_conditional());
      assert(cond_stmt != NULL);
      SgExpression * cond_expr = cond_stmt->get_expression();
      assert(cond_expr != NULL);

      std::vector<SgVarRefExp *> var_refs = SageInterface::querySubTree<SgVarRefExp>(cond_expr);
      std::vector<SgVarRefExp *>::const_iterator it_var_ref;
      for (it_var_ref = var_refs.begin(); it_var_ref != var_refs.end(); it_var_ref++) {
        SgVariableSymbol * sym = (*it_var_ref)->get_symbol();
        if (std::find(iterators.begin(), iterators.end(), sym) == iterators.end())
          others.push_back(sym); 
      }

      LoopTrees::cond_t * cond = new LoopTrees::cond_t(cond_expr);
      
      cond->block_true = buildLoopTreeBlock(if_stmt->get_true_body(), loop_tree, iterators, others, loop_map);
      cond->block_false = buildLoopTreeBlock(if_stmt->get_false_body(), loop_tree, iterators, others, loop_map);
      
      return cond;
    }
    case V_SgExprStatement:
    {
      SgExprStatement * expr_stmt = (SgExprStatement *)stmt;
      SgExpression * expr = expr_stmt->get_expression();
      assert(expr != NULL);

      std::vector<SgVarRefExp *> var_refs = SageInterface::querySubTree<SgVarRefExp>(expr);
      std::vector<SgVarRefExp *>::const_iterator it_var_ref;
      for (it_var_ref = var_refs.begin(); it_var_ref != var_refs.end(); it_var_ref++) {
        SgVariableSymbol * sym = (*it_var_ref)->get_symbol();
        if (std::find(iterators.begin(), iterators.end(), sym) == iterators.end())
          others.push_back(sym); 
      }

      return new LoopTrees::stmt_t(stmt);
    }
    default:
      std::cerr << "Unsupported statement : " << stmt->class_name() << " ( " << stmt << " )" << std::endl;
      assert(false);
  }

  assert(false);
  return NULL;
}

void extractLoopTrees(
  const std::vector<Directives::directive_t<OpenACC::language_t> *> & directives,
  std::map<Directives::directive_t<OpenACC::language_t> *, LoopTrees *> & regions,
  std::map<SgForStatement *, LoopTrees::loop_t *> & loop_map
) {
  std::vector<Directives::directive_t<OpenACC::language_t> *>::const_iterator it_directive;
  for (it_directive = directives.begin(); it_directive != directives.end(); it_directive++) {
    Directives::directive_t<OpenACC::language_t> * directive = *it_directive;
    switch (directive->construct->kind) {
      case OpenACC::language_t::e_acc_construct_parallel:
      {
        Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_parallel> * construct =
                 (Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_parallel> *)(directive->construct);

        LoopTrees * loop_tree = new LoopTrees();
        regions.insert(std::pair<Directives::directive_t<OpenACC::language_t> *, LoopTrees *>(directive, loop_tree));

        interpretClauses(directive->clause_list, loop_tree);

        SgStatement * region_base = construct->assoc_nodes.parallel_region;
        if (isSgPragmaDeclaration(region_base)) {
          assert(directive->successor_list.size() == 1);
          Directives::directive_t<OpenACC::language_t> * child = directive->successor_list.begin()->second;
          assert(child->construct->kind == OpenACC::language_t::e_acc_construct_loop);
          region_base = ((Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_loop> *)child->construct)->assoc_nodes.for_loop;
        }

       std::vector<SgVariableSymbol *> iterators;
       std::vector<SgVariableSymbol *> others;

        SgBasicBlock * region_bb = isSgBasicBlock(region_base);
        if (region_bb != NULL) {
           std::vector<SgStatement *>::const_iterator it_stmt;
           for (it_stmt = region_bb->get_statements().begin(); it_stmt != region_bb->get_statements().end(); it_stmt++)
             if (!isSgPragmaDeclaration(*it_stmt))
               loop_tree->addTree(buildLoopTree(*it_stmt, loop_tree, iterators, others, loop_map));
        }
        else loop_tree->addTree(buildLoopTree(region_base, loop_tree, iterators, others, loop_map));

        const std::vector<SgVariableSymbol *> & params = loop_tree->getParameters();
        const std::vector<KLT::Data<Annotation> *> & datas_ = loop_tree->getDatas();

        std::vector<SgVariableSymbol *> datas;
        std::vector<KLT::Data<Annotation> *>::const_iterator it_data;
        for (it_data = datas_.begin(); it_data != datas_.end(); it_data++)
          datas.push_back((*it_data)->getVariableSymbol());

        std::vector<SgVariableSymbol *>::const_iterator it_other;
        for (it_other = others.begin(); it_other != others.end(); it_other++)
          if (std::find(params.begin(), params.end(), *it_other) == params.end() && std::find(datas.begin(), datas.end(), *it_other) == datas.end())
            loop_tree->addScalar(*it_other); // Neither iterators or parameters or data

        loop_tree->toText(std::cout);

        break;
      }
      case OpenACC::language_t::e_acc_construct_kernel:
      {
        assert(false); /// \todo generate LoopTrees from kernel regions
        break;
      }
      case OpenACC::language_t::e_acc_construct_data:
      case OpenACC::language_t::e_acc_construct_loop:
      case OpenACC::language_t::e_acc_construct_host_data:
      case OpenACC::language_t::e_acc_construct_declare:
      case OpenACC::language_t::e_acc_construct_cache:
      case OpenACC::language_t::e_acc_construct_update:
      case OpenACC::language_t::e_acc_construct_blank:
      default:
        break;
    }
  }
}

SgBasicBlock * buildRegionBlock(LoopTrees * loop_trees, const DLX::OpenACC::compiler_modules_t::libopenacc_api_t & libopenacc_api) {
  SgBasicBlock * result = SageBuilder::buildBasicBlock();

  // build decl : acc_region_t region = acc_build_region(0);

  SgInitializer * init = SageBuilder::buildAssignInitializer(
    SageBuilder::buildFunctionCallExp(
      SageBuilder::buildFunctionRefExp(libopenacc_api.build_region),
      SageBuilder::buildExprListExp(SageBuilder::buildIntVal(loop_trees->id))
    )
  );
  /// \todo notify MFB that we use libopenacc_api.region_class->node->symbol
  SgVariableDeclaration * region_decl = SageBuilder::buildVariableDeclaration("region", SageBuilder::buildPointerType(libopenacc_api.region_class->node->symbol->get_type()), init, result);
  SageInterface::appendStatement(region_decl, result);

  SgVariableSymbol * region_sym = SageInterface::getFirstVarSym(region_decl);
  assert(region_sym != NULL);

    // set gang/worker/vector info
    // set data info
    // set loop info

  // build call : acc_push_data_environment();

  // data : copyin / create

  // build call : acc_region_execute(region);

  // data : copyout

  // build call : acc_pop_data_environment();

  return result;
}

template <>
bool Compiler<DLX::OpenACC::language_t, DLX::OpenACC::compiler_modules_t>::compile(
  const Compiler<DLX::OpenACC::language_t, DLX::OpenACC::compiler_modules_t>::directives_ptr_set_t & directives,
  const Compiler<DLX::OpenACC::language_t, DLX::OpenACC::compiler_modules_t>::directives_ptr_set_t & graph_entry,
  const Compiler<DLX::OpenACC::language_t, DLX::OpenACC::compiler_modules_t>::directives_ptr_set_t & graph_final
) {
  /// \todo verify that it is correct OpenACC.....

  /// Extract LoopTrees from original code
  std::map<Directives::directive_t<OpenACC::language_t> *, LoopTrees *> regions;
  std::map<SgForStatement *, LoopTrees::loop_t *> loop_map;
  extractLoopTrees(directives, regions, loop_map);

  std::vector<Directives::directive_t<OpenACC::language_t> *>::const_iterator it_directive;
  for (it_directive = directives.begin(); it_directive != directives.end(); it_directive++) {
    Directives::directive_t<OpenACC::language_t> * directive = *it_directive;
    if (directive->construct->kind == OpenACC::language_t::e_acc_construct_loop) {
      Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_loop> * loop_construct =
                 (Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_loop> *)(directive->construct);
      std::map<SgForStatement *, LoopTrees::loop_t *>::const_iterator it_loop = loop_map.find(loop_construct->assoc_nodes.for_loop);
      assert(it_loop != loop_map.end());
      std::vector<Directives::generic_clause_t<OpenACC::language_t> *>::const_iterator it_clause;
      for (it_clause = directive->clause_list.begin(); it_clause != directive->clause_list.end(); it_clause++) 
        it_loop->second->annotations.push_back(KLT_Annotation<OpenACC::language_t>(*it_clause));
    }
  }

  /// Generate kernels
  size_t region_cnt = 0;
  std::map<Directives::directive_t<OpenACC::language_t> *, LoopTrees *>::const_iterator it_region;
  for (it_region = regions.begin(); it_region != regions.end(); it_region++) {
    MDCG::OpenACC::RegionDesc::input_t input_region;
      input_region.id = it_region->second->id;
      input_region.file = compiler_modules.ocl_kernels_file;
      input_region.loop_tree = it_region->second;

    compiler_modules.generator.generate(*(it_region->second), input_region.kernel_lists, compiler_modules.cg_config);

    compiler_modules.comp_data.regions.push_back(input_region);
  }

  /// Generate host data
  compiler_modules.codegen.addDeclaration<MDCG::OpenACC::CompilerData>(compiler_modules.compiler_data_class, compiler_modules.comp_data, compiler_modules.host_data_file_id, "compiler_data");

//  MDCG::OpenACC::CompilerData::storeToDB(compiler_modules.versions_db_file, compiler_modules.comp_data);

  /// Transforms parallel/kernel directives
  for (it_region = regions.begin(); it_region != regions.end(); it_region++) {
    SgBasicBlock * region_block = buildRegionBlock(it_region->second, compiler_modules.libopenacc_api);

    SgStatement * old_region = NULL;

    if (it_region->first->construct->kind == OpenACC::language_t::e_acc_construct_parallel) {
      old_region = ((Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_parallel> *)(it_region->first->construct))->assoc_nodes.parallel_region;
      ((Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_parallel> *)(it_region->first->construct))->assoc_nodes.parallel_region = region_block;
    }
    else if (it_region->first->construct->kind == OpenACC::language_t::e_acc_construct_kernel) {
      old_region = ((Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_kernel> *)(it_region->first->construct))->assoc_nodes.kernel_region;
      ((Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_kernel> *)(it_region->first->construct))->assoc_nodes.kernel_region = region_block;
    }
    assert(old_region != NULL);

    if (isSgPragmaDeclaration(old_region)) {
      assert(it_region->first->successor_list.size() == 1);
      Directives::directive_t<OpenACC::language_t> * child = it_region->first->successor_list.begin()->second;
      assert(child->construct->kind == OpenACC::language_t::e_acc_construct_loop);
      old_region = ((Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_loop> *)child->construct)->assoc_nodes.for_loop;
    }

    SageInterface::replaceStatement(old_region, region_block);
  }

  /// Transforms data constructs
  for (it_directive = directives.begin(); it_directive != directives.end(); it_directive++) {
    Directives::directive_t<OpenACC::language_t> * directive = *it_directive;
    if (directive->construct->kind == OpenACC::language_t::e_acc_construct_data) {
      //assert(false); /// \todo
    }
  }

  std::vector<SgPragmaDeclaration * > pragma_decls = SageInterface::querySubTree<SgPragmaDeclaration>(compiler_modules.driver.project);
  std::vector<SgPragmaDeclaration * >::iterator it_pragma_decl;
  for (it_pragma_decl = pragma_decls.begin(); it_pragma_decl != pragma_decls.end(); it_pragma_decl++) {
    std::string directive_string = (*it_pragma_decl)->get_pragma()->get_pragma();
    if (::DLX::Frontend::consume_label(directive_string, OpenACC::language_t::language_label))
      SageInterface::removeStatement(*it_pragma_decl);
  }

  return true;
}

/** @} */
}

}

