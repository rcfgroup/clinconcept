library(readr)
get_ctable_name.NHSReadV2<-function(dict) {
  "read_version2"
}
get_ctable_code_field.NHSReadV2<-function(dict) {
  "read_code"
}
get_ctable_term_field.NHSReadV2<-function(dict) {
  "term"
}
build_concept_tables.sqlite.NHSReadV2 <- function(dict,replacements) {
  #read CSV data into SQLite table
  read2<-read_csv(paste0(replacements[['data-file-path']],"/V2/Unified/Corev2.all"),
                         col_names=c("read_code","term_30","term_60","term_198","X1","X2","X3","X4","X5","X6","X7","X8","X9"))
  dbWriteTable(dict$src,"read_version2",read2)
}
get_child_codes.NHSReadV2<-function(dict,code,immediate_children=F,current_only=F) {
  children<-collect(extract_relations_from_hierarchy(dict,code,immediate_relations=immediate_children,children=T))
  children$read_code
}
get_parent_codes.NHSReadV2<-function(dict,code,immediate_parents=F,current_only=F) {
  data<-collect(extract_relations_from_hierarchy(dict,code,immediate_relations=immediate_parents,children=F))
  data$read_code
}
get_relationships.NHSReadV2<-function(dict,code,children) {
  relations<-extract_relations_from_hierarchy(dict,code,immediate_relations=F,children=children)
  #%>% select(c("read_code")) %>% collapse()
  #parents<-substr(relations$read_code,0,1)
  #relations<-cbind(relations,parent_read_code=parents)
  #relations
}
