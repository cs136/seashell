function check_syntax(code, result_cb)
{
    var error_list = [{
            line_no: 1,
            column_no: 1,
            fragment: "#include:",
            message: "invalid syntax"
        }]

    result_cb(error_list);
}