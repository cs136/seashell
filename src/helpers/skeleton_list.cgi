#!/bin/bash
# This files lists the available assignment skeletons, for seashell. Each entry
# in the JSON array printed by this file should correspond to a directory (git
# repository) in ~cs136/public_html/assignment_skeletons/
TEMPLATE_FILE=/u/cs145/public_html/assignnment_skeletons/skeletons.json

echo "Content-Type: text/json"
echo "Access-Control-Allow-Origin: *"
echo ""
# example: '["A0", "A1", "A2"]'

cat $TEMPLATE_FILE

# W15:
#echo '["A0", "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8","A9","A10","Tut04", "Tut05", "Tut06", "Tut07", "Tut08", "Tut09", "Tut10", "Tut11"]'
