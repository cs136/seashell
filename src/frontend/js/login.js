function submit_login() {
  $.post("login.cgi",
         $("#loginform").serialize(),
         function(data, textStatus, jqXHR) {
           console.log(data);
         }
  );
}
