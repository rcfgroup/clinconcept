library(rclinconcept)

write_lines_to_file<-function(line_vector,filename) {
  fileConn<-file(filename)
  writeLines(line_vector, fileConn)
  close(fileConn)
}

dict_sm_mysql<-rcc_from_list("NHSSnomedCT",list(type="mysql",dbname="rcc_clinical_coding", user="root",pass="password"))
codes<-get_child_codes(dict_sm_mysql,"389145006")
codes<-c(codes,"389145006")

target_dir<-paste0(getwd(),"/tests/testthat/data/NHSSnomedCT/Full/Terminology")
D<-dplyr::tbl(dict_sm_mysql$src,"snomed_description") %>% filter(snomed_code %in% codes) %>% collect()
D$row_names<-NULL
write.table(D,paste0(target_dir,"/sct2_Description_Full-en_INT_20170131.txt"),row.names=F,sep="\t",quote=F)
E<-dplyr::tbl(dict_sm_mysql$src,"snomed_relationship") %>% filter(snomed_code %in% codes | parent_snomed_code %in% codes) %>% collect()
E$row_names<-NULL
write.table(E,paste0(target_dir,"/sct2_Relationship_Full-en_INT_20170131.txt"), row.names=F,sep="\t",quote=F)

