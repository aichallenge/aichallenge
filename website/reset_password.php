<?php
ini_set('error_reporting', E_ALL);
ini_set('display_errors', true);

require_once('mysql_login.php');

if (isset($argv[1])) {
	$username = mysql_real_escape_string(stripslashes($argv[1]));
	echo "Enter new password: ";
	$password1 = mysql_real_escape_string(stripslashes(str_replace(array("\r","\n"), "", fgets(STDIN))));
	echo "Retype new password: ";
	$password2 = mysql_real_escape_string(stripslashes(str_replace(array("\r","\n"), "", fgets(STDIN))));
	
	if ($password1 == $password2) {
		$passhash = crypt($password1, '$6$rounds=54321$' . salt() . '$');
		echo "Password hash is " . $passhash . "\n";
		$sql = "update user
		        set password = '" . $passhash ."'
		        where username = '" . $username . "'";
		if (mysql_query($sql)) {
			echo "Password has been changed\n";
			if (check_credentials($username, $password1)) {
				echo "Password verified.\n";
			} else {
				echo "Password not verified.\n";
			}
		} else {
			echo "Database error.\n";
		}
	} else {
		echo "Sorry, passwords do not match\n";
	}
} else {
	echo "Usage: php reset_password.php <username>\n";
}
?>