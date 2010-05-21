<?php

function contains_bad_word($word) {
  $word = strtolower($word);
  if (strpos($word, "fuck") !== FALSE) return true;
  if (strpos($word, "shit") !== FALSE) return true;
  if (strpos($word, "dick") !== FALSE) return true;
  if (strpos($word, "cunt") !== FALSE) return true;
  if (strpos($word, "gay") !== FALSE) return true;
  if (strpos($word, "sex") !== FALSE) return true;
  if (strpos($word, "penis") !== FALSE) return true;
  if (strpos($word, "vagina") !== FALSE) return true;
  if (strpos($word, "tits") !== FALSE) return true;
  if (strpos($word, "asshole") !== FALSE) return true;
  return false;
}

?>
