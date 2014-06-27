
#include "KLT/Core/data-flow.hpp"

#include "openacc_spec.hpp"

namespace KLT {

struct splitted_access_desc_t {
   typedef enum {read,write} rw_e;

   rw_e rw;

   ::KLT::LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::loop_t * splitted_loop;
   ::DLX::Directives::clause_t< ::DLX::OpenACC::language_t, ::DLX::OpenACC::language_t::e_acc_clause_split> * split_clause;

   const std::vector<SgExpression *> * subscripts;

   splitted_access_desc_t(
     rw_e rw_,
     ::KLT::LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::loop_t * splitted_loop_,
     ::DLX::Directives::clause_t< ::DLX::OpenACC::language_t, ::DLX::OpenACC::language_t::e_acc_clause_split> * split_clause_,
     const std::vector<SgExpression *> * subscripts_
   ) :
     rw(rw_),
     splitted_loop(splitted_loop_),
     split_clause(split_clause_),
     subscripts(subscripts_)
   {}
};

template <>
void DataFlow<
  DLX::KLT_Annotation<DLX::OpenACC::language_t>,
  Language::OpenCL,
  Runtime::OpenACC
>::markSplittedData(
  const context_t & context
) const {
  std::map< ::KLT::Data<Annotation> *, std::vector<splitted_access_desc_t> > accesses_map;
  std::map< ::KLT::Data<Annotation> *, std::vector<splitted_access_desc_t> >::iterator it_accesses;

  std::map< ::KLT::LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::node_t *, accesses_list_t>::const_iterator it;
  for (it = context.accesses_map.begin(); it != context.accesses_map.end(); it++) {
    ::KLT::LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::node_t * node = it->first;
    ::KLT::LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::node_t * parent = node->parent;

    ::KLT::LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::loop_t * splitted_loop = NULL;
    ::DLX::Directives::clause_t< ::DLX::OpenACC::language_t, ::DLX::OpenACC::language_t::e_acc_clause_split> * split_clause = NULL;

    // search for a splitted loop in the parents
    while (parent != NULL) {
      ::KLT::LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::loop_t * loop = dynamic_cast< ::KLT::LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::loop_t *>(parent);
      if (loop != NULL && loop->isSplitted()) {
        assert(splitted_loop == NULL); // cannot have nested splitted loops
        splitted_loop = loop;

        std::vector<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::const_iterator it;
        for (it = loop->annotations.begin(); it != loop->annotations.end(); it++) {
          if (it->clause->kind == DLX::OpenACC::language_t::e_acc_clause_split) {
            assert(split_clause == NULL); // only one split clause per loop
            split_clause = (::DLX::Directives::clause_t< ::DLX::OpenACC::language_t, ::DLX::OpenACC::language_t::e_acc_clause_split> *)it->clause;
          }
        }
        assert(split_clause != NULL);
      }
      parent = parent->parent;
    }
    assert(splitted_loop == NULL || split_clause != NULL); // (loop != NULL) => (split_clause != NULL)

    std::vector<data_access_t>::const_iterator it_data_access;
    for (it_data_access = it->second.reads.begin(); it_data_access != it->second.reads.end(); it_data_access++) {
      it_accesses = accesses_map.find(it_data_access->data);
      if (it_accesses == accesses_map.end())
        it_accesses = accesses_map.insert(std::pair< ::KLT::Data<Annotation> *, std::vector<splitted_access_desc_t> >(it_data_access->data, std::vector<splitted_access_desc_t>())).first;
      assert(it_accesses != accesses_map.end());
      it_accesses->second.push_back(splitted_access_desc_t(splitted_access_desc_t::read, splitted_loop, split_clause, &(it_data_access->subscripts)));
    }
    for (it_data_access = it->second.writes.begin(); it_data_access != it->second.writes.end(); it_data_access++) {
      it_accesses = accesses_map.find(it_data_access->data);
      if (it_accesses == accesses_map.end())
        it_accesses = accesses_map.insert(std::pair< ::KLT::Data<Annotation> *, std::vector<splitted_access_desc_t> >(it_data_access->data, std::vector<splitted_access_desc_t>())).first;
      assert(it_accesses != accesses_map.end());
      it_accesses->second.push_back(splitted_access_desc_t(splitted_access_desc_t::write, splitted_loop, split_clause, &(it_data_access->subscripts)));
    }
  }

  for (it_accesses = accesses_map.begin(); it_accesses != accesses_map.end(); it_accesses++) {
    ::KLT::Data<Annotation> * data = it_accesses->first;
    assert(it_accesses->second.size() > 0);

    if (it_accesses->second.size() > 1) {
      std::vector<splitted_access_desc_t>::iterator it_splitted_access = it_accesses->second.begin();
      ::KLT::LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::loop_t * splitted_loop = it_splitted_access->splitted_loop;
      for (; it_splitted_access != it_accesses->second.end(); it_splitted_access++)
        assert(splitted_loop == it_splitted_access->splitted_loop);
    }

    if (it_accesses->second[0].splitted_loop == NULL) continue;

    assert(it_accesses->second[0].split_clause != NULL);
    assert(it_accesses->second[0].subscripts != NULL);
    assert(it_accesses->second[0].subscripts->size() > 0);

    SgVarRefExp * var_ref = isSgVarRefExp(it_accesses->second[0].subscripts->at(0));
    assert(var_ref != NULL);
    if (var_ref->get_symbol() != it_accesses->second[0].splitted_loop->iterator) {
      std::vector<splitted_access_desc_t>::iterator it_splitted_access;
      for (it_splitted_access = it_accesses->second.begin(); it_splitted_access != it_accesses->second.end(); it_splitted_access++)
        assert(it_splitted_access->rw == splitted_access_desc_t::read);
      continue;
    }

    ::KLT::Data<Annotation>::data_distribution_t * data_distribution = new ::KLT::Data<Annotation>::data_distribution_t();
      data_distribution->distributed_dimension = 0;
      switch (it_accesses->second[0].split_clause->parameters.kind) {
        case ::DLX::Directives::generic_clause_t< ::DLX::OpenACC::language_t>::parameters_t< ::DLX::OpenACC::language_t::e_acc_clause_split>::e_acc_split_contiguous:
        {
          data_distribution->kind = ::KLT::Data<Annotation>::data_distribution_t::e_split_contiguous;
          std::vector<SgExpression *>::const_iterator it_portion;
          for (it_portion = it_accesses->second[0].split_clause->parameters.portions.begin(); it_portion != it_accesses->second[0].split_clause->parameters.portions.end(); it_portion++) {
            SgIntVal * portion = isSgIntVal(*it_portion);
            if (portion == NULL) {
              std::cerr << "(*it_portion)->class_name() = " << (*it_portion)->class_name() << std::endl;
              assert(false);
            }
            data_distribution->portions.push_back(portion->get_value());
          }
          break;
        }
        case ::DLX::Directives::generic_clause_t< ::DLX::OpenACC::language_t>::parameters_t< ::DLX::OpenACC::language_t::e_acc_clause_split>::e_acc_split_chunk:
        {
            break;
        }
        default:
          assert(false);
      }
    data->setDistribution(data_distribution);
  }
}

}

