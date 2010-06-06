<?php

require_once("phpmailer/class.phpmailer.php");
require_once("phpmailer/class.smtp.php");

function send_gmail($recipient, $subject, $body) {
  include("server_info.php");
  if (strlen($server_info["mailer_address"]) < 4) {
    // The mailer address is clearly not right...
    return 0;
  }
  $mail = new PHPMailer();
  $mail->IsSMTP();
  $mail->SMTPAuth = true;
  $mail->Username = $server_info["mailer_address"];
  $mail->Password = $server_info["mailer_password"];
  $mail->From = $server_info["mailer_address"];
  $mail->FromName = $server_info["mailer_name"];
  $mail->AddAddress($recipient, "");
  $mail->WordWrap = 80;
  $mail->Host = "ssl://smtp.gmail.com";
  $mail->Port = 465;
  $mail->Subject = $subject;
  $mail->Body = $body;
  return $mail->Send();
}

?>
