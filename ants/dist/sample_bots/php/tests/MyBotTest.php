<?php
require_once 'PHPUnit/Framework.php';
require_once '../MyBot.php';

class MyBotTest extends PHPUnit_Framework_TestCase
{
    private $bot;
    private $ants;

    protected function setUp()
    {
        $this->ants = new Ants();
        $this->bot = new MyBot();
    }

    /**
     * Ant goes north if this direction isn't blocked
     */
    public function testAntGoesNorth()
    {
        $data = explode("\n", "rows 10\ncols 15\n");
        $this->ants->setup($data);

        $data = explode("\n", "a 2 2 0\nf 2 5");
        $this->ants->update($data);

        ob_start();
        $this->bot->doTurn($this->ants);
        $output = ob_get_clean();

        $this->assertEquals("o 2 2 n\n", $output);
    }

    /**
     * Ant goes east if north is blocked
     */
    public function testAntGoesEast()
    {
        $data = explode("\n", "rows 10\ncols 15\n");
        $this->ants->setup($data);

        $data = explode("\n", "a 2 2 0\nw 1 2\nw 1 3");
        $this->ants->update($data);

        ob_start();
        $this->bot->doTurn($this->ants);
        $output = ob_get_clean();

        $this->assertEquals("o 2 2 e\n", $output);
    }

    /**
     * Ant goes south if north and east is blocked
     */
    public function testAntGoesSouth()
    {
        $data = explode("\n", "rows 10\ncols 15\n");
        $this->ants->setup($data);

        $data = explode("\n", "a 2 2 0\nw 1 2\nw 1 3\nw 2 3");
        $this->ants->update($data);

        ob_start();
        $this->bot->doTurn($this->ants);
        $output = ob_get_clean();

        $this->assertEquals("o 2 2 s\n", $output);
    }

    /**
     * Ant goes west if north, east and south is blocked
     */
    public function testAntGoesWest()
    {
        $data = explode("\n", "rows 10\ncols 15\n");
        $this->ants->setup($data);

        $data = explode("\n", "a 2 2 0\nw 1 2\nw 1 3\nw 2 3\nw 3 2");
        $this->ants->update($data);

        ob_start();
        $this->bot->doTurn($this->ants);
        $output = ob_get_clean();

        $this->assertEquals("o 2 2 w\n", $output);
    }

    public function testEnclosedAntDoesntMove()
    {
        $data = explode("\n", "rows 10\ncols 15\n");
        $this->ants->setup($data);

        $data = explode("\n", "a 2 2 0\nw 1 2\nw 1 3\nw 2 3\nw 3 2\nw 2 1");
        $this->ants->update($data);

        ob_start();
        $this->bot->doTurn($this->ants);
        $output = ob_get_clean();

        $this->assertEquals("", $output);
    }

    public function testAllAntsMove()
    {
        $data = explode("\n", "rows 10\ncols 15\n");
        $this->ants->setup($data);

        $data = explode("\n", "a 2 2 0\nw 1 2\nw 1 3\nw 2 3\nw 3 2\na 4 2 0\na 7 7 0");
        $this->ants->update($data);

        ob_start();
        $this->bot->doTurn($this->ants);
        $output = ob_get_clean();

        $this->assertEquals("o 2 2 w\no 4 2 e\no 7 7 n\n", $output);
    }
}