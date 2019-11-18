library(rclinconcept)

write_lines_to_file<-function(line_vector,filename) {
  fileConn<-file(filename)
  writeLines(line_vector, fileConn)
  close(fileConn)
}

dict3_mysql<-rcc_from_list("NHSReadV2",list(type="mysql",dbname="clinical_coding", user="root",pass="password"))

codes<-get_child_codes(dict3_mysql,"H3...",output_codes=T)
codes<-c(codes,get_child_codes(dict3_mysql,"h3...",output_codes=T))
codes<-c(codes,get_child_codes(dict3_mysql,"E3...",output_codes=T))
codes<-c(codes,get_parent_codes(dict3_mysql,"H3...",output_codes=T))
codes<-c(codes,get_parent_codes(dict3_mysql,"h3...",output_codes=T))
codes<-c(codes,get_parent_codes(dict3_mysql,"E3...",output_codes=T))
codes<-c(codes,get_child_codes(dict3_mysql,"H31..",output_codes=T))
codes<-c(codes,get_child_codes(dict3_mysql,"h31..",output_codes=T))
codes<-c(codes,get_child_codes(dict3_mysql,"E31..",output_codes=T))
codes<-c(codes,get_parent_codes(dict3_mysql,"H31..",output_codes=T))
codes<-c(codes,get_parent_codes(dict3_mysql,"h31..",output_codes=T))
codes<-c(codes,get_parent_codes(dict3_mysql,"E31..",output_codes=T))
codes<-c(codes,get_child_codes(dict3_mysql,"H32..",output_codes=T))
codes<-c(codes,get_child_codes(dict3_mysql,"h32..",output_codes=T))
codes<-c(codes,get_parent_codes(dict3_mysql,"H32..",output_codes=T))
codes<-c(codes,get_parent_codes(dict3_mysql,"h32..",output_codes=T))
codes<-c(codes,"h3...","E3...","H3...","H31..","H32..")
print(unique(codes))
write_lines_to_file(unique(codes),paste0(getwd(),"/tests/testthat/data/NHSReadV3/read_code_list.csv"))
D<-dplyr::tbl(dict3_mysql$src,"read_version3") %>% filter(read_code %in% codes) %>% collect()
write_lines_to_file(D$term_id,paste0(getwd(),"/tests/testthat/data/NHSReadV3/term_id_list.csv"))

