<?php
function getPaginationString($page = 1, $lastpage, $perpage = 50, $pagestring = "?page=", $targetpage=NULL)
{
    //other vars
    if ($targetpage === NULL) {
        $targetpage = $_SERVER['PHP_SELF'];
    }
    $adjacents = 2;
    $prev = $page - 1;
    $next = $page + 1;
    $lpm1 = $lastpage - 1;
    $pagestring = ($pagestring == null)?"?page=":$pagestring;

    $pagination = "";
    if($lastpage > 1)
    {
        $pagination .= "<div class=\"pagination\">";

        if ($page > 1)
            $pagination .= "<span><a href=\"$targetpage$pagestring$prev\">&laquo; prev</a></span>";
        else
            $pagination .= "<span class=\"disabled\">&laquo; prev</span>";

        if ($lastpage < 5 + ($adjacents * 2))    //not enough pages to bother breaking it up
        {
            for ($counter = 1; $counter <= $lastpage; $counter++)
            {
                if ($counter == $page)
                    $pagination .= "<span class=\"current\">$counter</span>";
                else
                    $pagination .= "<span><a href=\"" . $targetpage . $pagestring . $counter . "\">$counter</a></span>";
            }
        }
        elseif($lastpage >= 5 + ($adjacents * 2))    //enough pages to hide some
        {
            if($page < 1 + ($adjacents * 3))
            {
                for ($counter = 1; $counter < 4 + ($adjacents * 2); $counter++)
                {
                    if ($counter == $page)
                        $pagination .= "<span class=\"current\">$counter</span>";
                    else
                        $pagination .= "<span><a href=\"" . $targetpage . $pagestring . $counter . "\">$counter</a></span>";
                }
                $pagination .= "<span class=\"elipses\">...</span>";
                $pagination .= "<span><a href=\"" . $targetpage . $pagestring . $lpm1 . "\">$lpm1</a></span>";
                $pagination .= "<span><a href=\"" . $targetpage . $pagestring . $lastpage . "\">$lastpage</a></span>";
            }
            elseif($lastpage - ($adjacents * 2) > $page && $page > ($adjacents * 2))
            {
                $pagination .= "<span><a href=\"" . $targetpage . $pagestring . "1\">1</a></span>";
                $pagination .= "<span><a href=\"" . $targetpage . $pagestring . "2\">2</a></span>";
                $pagination .= "<span class=\"elipses\">...</span>";
                for ($counter = $page - $adjacents; $counter <= $page + $adjacents; $counter++)
                {
                    if ($counter == $page)
                        $pagination .= "<span class=\"current\">$counter</span>";
                    else
                        $pagination .= "<span><a href=\"" . $targetpage . $pagestring . $counter . "\">$counter</a></span>";
                }
                $pagination .= "...";
                $pagination .= "<span><a href=\"" . $targetpage . $pagestring . $lpm1 . "\">$lpm1</a></span>";
                $pagination .= "<span><a href=\"" . $targetpage . $pagestring . $lastpage . "\">$lastpage</a></span>";
            }
            else
            {
                $pagination .= "<span><a href=\"" . $targetpage . $pagestring . "1\">1</a></span>";
                $pagination .= "<span><a href=\"" . $targetpage . $pagestring . "2\">2</a></span>";
                $pagination .= "<span class=\"elipses\">...</span>";
                for ($counter = $lastpage - (1 + ($adjacents * 3)); $counter <= $lastpage; $counter++)
                {
                    if ($counter == $page)
                        $pagination .= "<span class=\"current\">$counter</span>";
                    else
                        $pagination .= "<span><a href=\"" . $targetpage . $pagestring . $counter . "\">$counter</a></span>";
                }
            }
        }

        if ($page < $counter - 1)
            $pagination .= "<span><a href=\"" . $targetpage . $pagestring . $next . "\">next &raquo;</a></span>";
        else
            $pagination .= "<span class=\"disabled\">next &raquo;</span>";
        $pagination .= "</div>\n";
    }

    return $pagination;
}
?>
