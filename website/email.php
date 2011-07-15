<?php

require_once("ses.php");
require_once("server_info.php");

function send_email($recipient, $subject, $body) {
    $ses = new SimpleEmailService(
        $server_info["aws_accesskey"],
        $server_info["aws_secretkey"]);
    $msg = new SimpleEmailServiceMessage();
    $msg->setFrom($server_info["mailer_address"]);
    $msg->addTo($recipient);
    $msg->setSubject($subject);
    $msg->setMessageFromString($body);

    $send_result = $ses->sendEmail($msg);
    $logline = sprintf("%s - Sent email (%s) to %s (id %s)\n",
        date(DATE_ATOM), $subject, $recipient, $send_result["MessageId"]);
    error_log($logline, 3, $server_info["api_log"]);
}

?>
