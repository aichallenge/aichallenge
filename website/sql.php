<?php

$sql = array(
    "select_next_compile" => "select submission_id
                              from submission
                              where status = 20
                              order by submission_id asc
                              limit 1;",
    "update_submission_compiling" => "update submission
                                      set status = 24,
                                          worker_id = %s
                                      where submission_id = %s"
);

?>