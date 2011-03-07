<?php
$memcache = false;
if (extension_loaded ('memcache')) {
	try {
		$memcache = new Memcache;
		$memcache->connect('localhost', 11211);
	} catch (Exception $e) {}
}

?>