INPUT_PATH="/Users/rcf8/Holding/nhs_readctv3_23.0.0_20170401000001/V3"
OUTPUT_PATH="/Users/rcf8/Projects/rclinconcept/tests/testthat/data/NHSReadV3"
grep -F -f $OUTPUT_PATH/read_code_list.csv $INPUT_PATH/Descrip.v3 > $OUTPUT_PATH/V3/Descrip.v3
grep -F -f $OUTPUT_PATH/read_code_list.csv $INPUT_PATH/V3hier.v3 > $OUTPUT_PATH/V3/V3hier.v3
grep -F -f $OUTPUT_PATH/read_code_list.csv $INPUT_PATH/Concept.v3 > $OUTPUT_PATH/V3/Concept.v3
grep -F -f $OUTPUT_PATH/term_id_list.csv $INPUT_PATH/Terms.v3 > $OUTPUT_PATH/V3/Terms.v3
