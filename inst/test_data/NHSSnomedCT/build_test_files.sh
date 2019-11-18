INPUT_PATH="/Users/rcf8/Downloads/uk_sct2cl_23.0.0_20170401000001/SnomedCT_InternationalRF2_Production_20170131T120000/Full/Terminology"
OUTPUT_PATH="/Users/rcf8/Projects/rclinconcept/tests/testthat/data/NHSSnomedCT"
grep -F -f $OUTPUT_PATH/snomed_code_list.csv $INPUT_PATH/sct2_Description_Full-en_INT_20170131.txt > $OUTPUT_PATH/V3/sct2_Description_Full-en_INT_20170131.txt
grep -F -f $OUTPUT_PATH/snomed_code_list.csv $INPUT_PATH/sct2_Relationship_Full_INT_20170131.txt > $OUTPUT_PATH/V3/sct2_Relationship_Full_INT_20170131.txt

