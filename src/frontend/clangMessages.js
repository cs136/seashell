function check_syntax(code, result_cb)
{
    var error_list = [{
            line_no: 1,
            column_no_start: 0,
            column_no_stop: 8,
            fragment: "#include:",
            message: "tra ta ta"
        }, {
            line_no: 3,
            column_no_start: 4,
            column_no_stop: 10,
            fragment: "main()",
            message: "blah blah blah",
            severity: "warning"
        }];

    result_cb(error_list);
}