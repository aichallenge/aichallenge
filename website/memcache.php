<?php

$memcache = false;
if (extension_loaded ('memcache')) {
	try {
		$memcache = new Memcache;
		try {
			$memcache->connect('localhost', 11211);
			if (!$memcache->set('test', 'success')) {
				error_log("Connected to memcache, but can't write.");
				$memcache->close();
				$memcache = false;
			}
		} catch (Exception $e) {
			error_log("Failed to connect to memcache.");
			$memcache = false;
		}
	} catch (Exception $e) {
		error_log("Failed to create memcache object.");
		$memcache = false;
	}
}

?>