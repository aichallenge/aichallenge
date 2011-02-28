<?php
function getPaginationString($page = 1, $totalitems, $perpage = 50, $pagestring = "?page=")
{	
	//other vars
	$targetpage = $_SERVER['PHP_SELF'];
	$adjacents = 2;
	$prev = $page - 1;
	$next = $page + 1;
	$lastpage = ceil($totalitems / $perpage);
    $lpm1 = $lastpage - 1;
    $pagestring = ($pagestring == null)?"?page=":$pagestring;
	
	$pagination = "";
	if($lastpage > 1)
	{	
		$pagination .= "<div class=\"pagination\">";

		if ($page > 1) 
			$pagination .= "<a href=\"$targetpage$pagestring$prev\">&laquo; prev</a>";
		else
			$pagination .= "<span class=\"disabled\">&laquo; prev</span>";	
		
		if ($lastpage < 5 + ($adjacents * 2))	//not enough pages to bother breaking it up
		{	
			for ($counter = 1; $counter <= $lastpage; $counter++)
			{
				if ($counter == $page)
					$pagination .= "<span class=\"current\">$counter</span>";
				else
					$pagination .= "<a href=\"" . $targetpage . $pagestring . $counter . "\">$counter</a>";					
			}
		}
		elseif($lastpage >= 5 + ($adjacents * 2))	//enough pages to hide some
		{
			if($page < 1 + ($adjacents * 3))		
			{
				for ($counter = 1; $counter < 4 + ($adjacents * 2); $counter++)
				{
					if ($counter == $page)
						$pagination .= "<span class=\"current\">$counter</span>";
					else
						$pagination .= "<a href=\"" . $targetpage . $pagestring . $counter . "\">$counter</a>";					
				}
				$pagination .= "<span class=\"elipses\">...</span>";
				$pagination .= "<a href=\"" . $targetpage . $pagestring . $lpm1 . "\">$lpm1</a>";
				$pagination .= "<a href=\"" . $targetpage . $pagestring . $lastpage . "\">$lastpage</a>";		
			}
			elseif($lastpage - ($adjacents * 2) > $page && $page > ($adjacents * 2))
			{
				$pagination .= "<a href=\"" . $targetpage . $pagestring . "1\">1</a>";
				$pagination .= "<a href=\"" . $targetpage . $pagestring . "2\">2</a>";
				$pagination .= "<span class=\"elipses\">...</span>";
				for ($counter = $page - $adjacents; $counter <= $page + $adjacents; $counter++)
				{
					if ($counter == $page)
						$pagination .= "<span class=\"current\">$counter</span>";
					else
						$pagination .= "<a href=\"" . $targetpage . $pagestring . $counter . "\">$counter</a>";					
				}
				$pagination .= "...";
				$pagination .= "<a href=\"" . $targetpage . $pagestring . $lpm1 . "\">$lpm1</a>";
				$pagination .= "<a href=\"" . $targetpage . $pagestring . $lastpage . "\">$lastpage</a>";		
			}
			else
			{
				$pagination .= "<a href=\"" . $targetpage . $pagestring . "1\">1</a>";
				$pagination .= "<a href=\"" . $targetpage . $pagestring . "2\">2</a>";
				$pagination .= "<span class=\"elipses\">...</span>";
				for ($counter = $lastpage - (1 + ($adjacents * 3)); $counter <= $lastpage; $counter++)
				{
					if ($counter == $page)
						$pagination .= "<span class=\"current\">$counter</span>";
					else
						$pagination .= "<a href=\"" . $targetpage . $pagestring . $counter . "\">$counter</a>";					
				}
			}
		}
		
		if ($page < $counter - 1) 
			$pagination .= "<a href=\"" . $targetpage . $pagestring . $next . "\">next &raquo;</a>";
		else
			$pagination .= "<span class=\"disabled\">next &raquo;</span>";
		$pagination .= "</div>\n";
	}
	
	return $pagination;
}
?>
